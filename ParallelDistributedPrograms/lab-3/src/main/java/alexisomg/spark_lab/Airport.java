package alexisomg.spark_lab;

import java.io.Serializable;

public class Airport implements Serializable {
    private final int airportCode;
    private final String airportName;

    public Airport(int airportCode, String airportName) {
        this.airportCode = airportCode;
        this.airportName = airportName;
    }

    public String getAirportName() {
        return this.airportName;
    }

    public int getAirportCode() {
        return this.airportCode;
    }
}
