package alexisomg.join;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

import java.io.IOException;

public class FlightMapper extends Mapper <LongWritable, Text, Key, Text> {
    @Override
    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
        if (key.get() == Constants.COLUMNS_NAMES_INDEX) {
            return;
        }
        FlightReader flight = FlightReader.read(value.toString());
        if (flight.getDelay() > 0) {
            context.write(flight.getKey(), new Text(String.valueOf(flight.getDelay())));
        }
    }
}
