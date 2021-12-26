package alexisomg.lab5;

public class GetRequestResult {
    private final String url;
    private final int requestCnt;

    public GetRequestResult(String url, int requestCnt) {
        this.url = url;
        this.requestCnt = requestCnt;
    }

    public int getRequestCnt() {
        return requestCnt;
    }

    public String getUrl() {
        return url;
    }
}
