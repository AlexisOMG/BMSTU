package alexisomg.spark_lab;

import java.io.Serializable;

public class AirportPairStatistic implements Serializable {
    private float maxDelay;
    private int cntDelayedOrCancelledFlights;
    private int cntFlights;

    public AirportPairStatistic(Flight flight) {
        this.maxDelay = flight.getDelay();
        this.cntFlights = 1;
        this.cntDelayedOrCancelledFlights = (flight.getDelay() != 0 || flight.wasCancelled() ? 1 : 0);
    }

    public static AirportPairStatistic addFlightToStatistic(AirportPairStatistic statistic, Flight flight) {
        statistic.maxDelay = Math.max(statistic.maxDelay, flight.getDelay());
        statistic.cntFlights += 1;
        if (flight.getDelay() != 0 || flight.wasCancelled()) {
            statistic.cntDelayedOrCancelledFlights += 1;
        }
        return statistic;
    }

    public static AirportPairStatistic merge(AirportPairStatistic first, AirportPairStatistic second) {
        first.maxDelay =  Math.max(first.maxDelay, second.maxDelay);
        first.cntDelayedOrCancelledFlights += second.cntDelayedOrCancelledFlights;
        first.cntFlights = first.cntFlights + second.cntFlights;
        return first;
    }

    public String getAirportPairStatistic() {
        return String.format("Max delay: %f, flights with delay or cancellation: %f%%",
                this.maxDelay,
                100.0 * this.cntDelayedOrCancelledFlights / this.cntFlights
        );
    }
}
