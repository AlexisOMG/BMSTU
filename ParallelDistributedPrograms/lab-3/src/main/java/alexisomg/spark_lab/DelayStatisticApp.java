package alexisomg.spark_lab;

import org.apache.spark.SparkConf;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.api.java.JavaSparkContext;
import org.apache.spark.broadcast.Broadcast;
import scala.Tuple2;

import java.util.Map;
import java.util.Objects;

public class DelayStatisticApp {
    public static final String DATA_SEPARATOR = ",";
    public static final int DELAY_POS = 18;
    public static final int DEPARTURE_AIRPORT_CODE_POS = 11;
    public static final int DESTINATION_AIRPORT_CODE_POS = 14;
    public static final int CANCELLATION_POS = 20;
    public static final int AIRPORT_CODE_POS = 0;
    public static final int AIRPORT_NAME_POS = 1;
    public static final String PATH_TO_AIRPORTS = "L_AIRPORT_ID.csv";
    public static final String PATH_TO_FLIGHTS = "664600583_T_ONTIME_sample.csv";

    public static void main(String[] args) {
        SparkConf conf = new SparkConf().setAppName("lab3");
        JavaSparkContext ctx = new JavaSparkContext(conf);

        JavaRDD <String> airportsData = ctx.textFile(PATH_TO_AIRPORTS);
        final String firstAirportsRow = airportsData.first();
        airportsData = airportsData
                .filter(str -> !Objects.equals(str, firstAirportsRow))
                .map(str -> str.replaceAll("\"", ""));

        JavaRDD <String> flightsData = ctx.textFile(PATH_TO_FLIGHTS);
        final String firstFlightRow = flightsData.first();
        flightsData = flightsData
                .filter(str -> !Objects.equals(str, firstFlightRow))
                .map(str -> str.replaceAll("\"", ""));

        JavaPairRDD<Tuple2<Integer, Integer>, Flight> flightsInfo = flightsData.mapToPair(str -> {
            String[] data = str.split(DATA_SEPARATOR);
            int departureAirportCode = Integer.parseInt(data[DEPARTURE_AIRPORT_CODE_POS]);
            int destinationAirportCode = Integer.parseInt(data[DESTINATION_AIRPORT_CODE_POS]);
            float delay = 0;
            if (!data[DELAY_POS].isEmpty()) {
                delay = Float.parseFloat(data[DELAY_POS]);
            }
            boolean isCancelled = !data[CANCELLATION_POS].isEmpty();
            return new Tuple2<>(new Tuple2<>(departureAirportCode, destinationAirportCode), new Flight(delay, isCancelled));
        });

        JavaPairRDD<Tuple2<Integer, Integer>, AirportPairStatistic> aggregatedData = flightsInfo.combineByKey(
                AirportPairStatistic::new,
                AirportPairStatistic::addFlightToStatistic,
                AirportPairStatistic::merge
        );

        Map <Integer, String> airportsNames = airportsData.mapToPair(str -> {
            String[] airportData = str.split(DATA_SEPARATOR, 2);
            Integer airportCode = Integer.parseInt(airportData[AIRPORT_CODE_POS]);
            String airportName = airportData[AIRPORT_NAME_POS];
           return new Tuple2<>(airportCode, airportName);
        }).collectAsMap();
        final Broadcast<Map<Integer, String>> airportsNameBroadcast = ctx.broadcast(airportsNames);

        JavaRDD<String> result = aggregatedData.map(statisticData -> {
            String stats = statisticData._2.getAirportPairStatistic();
            String departureAirportName = airportsNameBroadcast.getValue().get(statisticData._1._1);
            String destinationAirportName = airportsNameBroadcast.getValue().get(statisticData._1._2);
            return String.format("Departure Airport Name: %s | Destination Airport Name: %s\n Statistic: %s",
                    departureAirportName,
                    destinationAirportName,
                    stats
            );
        });

        result.saveAsTextFile("result");
    }
}
