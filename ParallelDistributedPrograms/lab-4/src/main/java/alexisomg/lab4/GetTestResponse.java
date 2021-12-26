package alexisomg.lab4;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class GetTestResponse {
    private final String packageID;
    private final List<TestingResult> results;

    @JsonCreator
    public GetTestResponse(
            @JsonProperty("packageId") String packageID,
            @JsonProperty("results") List<TestingResult> results
    ) {
        this.packageID = packageID;
        this.results = results;
    }

    public String getPackageID() {
        return this.packageID;
    }
    public List<TestingResult> getResults() {
        return this.results;
    }
}
