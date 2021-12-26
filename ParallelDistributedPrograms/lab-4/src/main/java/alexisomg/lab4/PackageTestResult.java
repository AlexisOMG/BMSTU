package alexisomg.lab4;

public class PackageTestResult {
    private final String packageID;
    private final TestingResult result;

    public PackageTestResult(
            String packageID,
            String status,
            String name,
            String expectedRes,
            String actualRes
    ) {
        this.packageID = packageID;
        this.result = new TestingResult(status, name, expectedRes, actualRes);
    }

    public String getPackageID() {
        return this.packageID;
    }
    public TestingResult getResult() {
        return this.result;
    }

    @Override
    public String toString() {
        return String.format("Package ID: %s\n%s", this.packageID, this.result);
    }
}
