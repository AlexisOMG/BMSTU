package alexisomg.lab4;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class PostTestRequest {
    private final String packageID;
    private final String script;
    private final String funcName;
    private final List<TestBody> testBodies;

    @JsonCreator
    public PostTestRequest(
            @JsonProperty("packageId") String packageID,
            @JsonProperty("jsScript") String script,
            @JsonProperty("functionName") String funcName,
            @JsonProperty("tests") List<TestBody> testBodies
    ) {
        this.packageID = packageID;
        this.funcName = funcName;
        this.script = script;
        this.testBodies = testBodies;
    }

    public String getPackageID() {
        return packageID;
    }

    public String getFuncName() {
        return funcName;
    }

    public String getScript() {
        return script;
    }

    public List<TestBody> getTestBodies() {
        return testBodies;
    }
}
