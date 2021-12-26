package alexisomg.lab4;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class TestingResult {
    private final String status;
    private final String name;
    private final String expectedRes;
    private final String actualRes;

    @JsonCreator
    public TestingResult(
            @JsonProperty("status") String status,
            @JsonProperty("testName") String name,
            @JsonProperty("expectedResult") String expectedRes,
            @JsonProperty("actualResult") String actualRes
    ) {
        this.status = status;
        this.name = name;
        this.expectedRes = expectedRes;
        this.actualRes = actualRes;
    }

    public String getStatus() {
        return this.status;
    }
    public String getName() {
        return this.name;
    }
    public String getExpectedRes() {
        return this.expectedRes;
    }
    public String getActualRes() {
        return this.actualRes;
    }

    @Override
    public String toString() {
        return String.format("Status: %s\nTest Name: %s\nExpected Result: %s\nActual Result: %s\n",
                this.status,
                this.name,
                this.expectedRes,
                this.actualRes
        );
    }
}
