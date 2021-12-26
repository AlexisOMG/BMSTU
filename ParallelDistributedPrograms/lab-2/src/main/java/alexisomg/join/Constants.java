package alexisomg.join;

public class Constants {
    public static final String USAGE_MESSAGE = "Usage: JoinApp <path_to_airports.csv> <path_to_flights.csv> <output>";
    public static final int ARGS_LENGTH = 3;

    public static final int COLUMNS_NAMES_INDEX = 0;
    public static final String DATA_SEPARATOR = ",";

    public static final int FLIGHT_FLAG = 1;
    public static final int AIRPORT_FLAG = 0;

    public static final int AIRPORTS_INPUT_PATH_POS = 0;
    public static final int FLIGHTS_INPUT_PATH_POS = 1;
    public static final int OUTPUT_PATH_POS = 2;

    public static final int NUM_REDUCE_TASKS = 2;
}
