package alexisomg.join;

public class FlightReader {
    private static final int DELAY_POS = 18;
    private static final int AIRPORT_POS = 14;

    private final float delay;
    private final int airportCode;

    public FlightReader(float delay, int airportCode) {
        this.delay = delay;
        this.airportCode = airportCode;
    }

    public float getDelay() {
        return this.delay;
    }

    public Key getKey() {
        return new Key(this.airportCode, Constants.FLIGHT_FLAG);
    }

    public static FlightReader read(String data) {
        String[] values = data.split(Constants.DATA_SEPARATOR);

        float delay = 0;
        if (!values[DELAY_POS].isEmpty()) {
            delay = Float.parseFloat(values[DELAY_POS]);
        }

        int airportCode = Integer.parseInt(values[AIRPORT_POS]);

        return new FlightReader(delay, airportCode);
    }
}
