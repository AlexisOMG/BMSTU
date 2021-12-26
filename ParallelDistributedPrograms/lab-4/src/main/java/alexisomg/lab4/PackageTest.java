package alexisomg.lab4;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class PackageTest {
    private final String packageID;
    private final String script;
    private final String funcName;
    private final TestBody testBody;

    @JsonCreator
    public PackageTest(
            @JsonProperty("packageId") String packageID,
            @JsonProperty("jsScript") String script,
            @JsonProperty("functionName") String funcName,
            @JsonProperty("tests") TestBody testBody
    ) {
        this.packageID = packageID;
        this.funcName = funcName;
        this.script = script;
        this.testBody = testBody;
    }

    public String getScript() {
        return script;
    }

    public String getFuncName() {
        return funcName;
    }

    public String getPackageID() {
        return packageID;
    }

    public TestBody getTestBody() {
        return testBody;
    }
}
