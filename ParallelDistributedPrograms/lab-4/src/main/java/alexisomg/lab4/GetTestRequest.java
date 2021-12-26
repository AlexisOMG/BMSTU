package alexisomg.lab4;

public class GetTestRequest {
    public final String packageID;

    public GetTestRequest(String packageID) {
        this.packageID = packageID;
    }

    public String getPackageID() {
        return packageID;
    }
}
