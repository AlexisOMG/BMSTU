package alexisomg.join;

public class AirportReader {
    private static final int AIRPORT_CODE_POS = 0;
    private static final int AIRPORT_NAME_POS = 1;
    private static final int MIN_QUOTED_STRING_LENGTH = 2;
    private static final int SPLIT_LIMIT = 2;

    private final int airportCode;
    private final String airportName;

    public AirportReader(int airportCode, String airportName) {
        this.airportCode = airportCode;
        this.airportName = airportName;
    }

    public static AirportReader read(String data) {
        String[] values = data.split(Constants.DATA_SEPARATOR, SPLIT_LIMIT);

        int airportCode = Integer.parseInt(removeQuotes(values[AIRPORT_CODE_POS]));
        String airportName = removeQuotes(values[AIRPORT_NAME_POS]);

        return new AirportReader(airportCode, airportName);
    }

    private static String removeQuotes(String value) {
        if (value.length() >= MIN_QUOTED_STRING_LENGTH) {
            return value.substring(1, value.length()-1);
        }
        return "";
    }

    public String getAirportName() {
        return this.airportName;
    }

    public Key getKey() {
        return new Key(this.airportCode, Constants.AIRPORT_FLAG);
    }
}
