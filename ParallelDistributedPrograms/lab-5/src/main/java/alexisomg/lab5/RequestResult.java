package alexisomg.lab5;

public class RequestResult {
    private final String url;
    private final int requestCnt;
    private final long requestTime;

    public RequestResult(String url, int requestCnt, long requestTime) {
        this.url = url;
        this.requestCnt = requestCnt;
        this.requestTime = requestTime;
    }

    public String getUrl() {
        return url;
    }

    public int getRequestCnt() {
        return requestCnt;
    }

    public long getRequestTime() {
        return requestTime;
    }

    public RequestResult addRequestResult(RequestResult res) {
        return new RequestResult(
                res.url,
                this.requestCnt + res.requestCnt,
                this.requestTime + res.requestTime
        );
    }
}
