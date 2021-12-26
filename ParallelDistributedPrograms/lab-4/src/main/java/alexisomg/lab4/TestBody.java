package alexisomg.lab4;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TestBody {
    private final String name;
    private final Object[] args;
    private final String expectedRes;

    @JsonCreator
    public TestBody(
            @JsonProperty("testName") String name,
            @JsonProperty("params") Object[] args,
            @JsonProperty("expectedResult") String expectedRes
    ) {
        this.name = name;
        this.args = args;
        this.expectedRes = expectedRes;
    }

    public String getName() {
        return name;
    }
    public Object[] getArgs() {
        return args;
    }
    public String getExpectedRes() {
        return expectedRes;
    }
}
