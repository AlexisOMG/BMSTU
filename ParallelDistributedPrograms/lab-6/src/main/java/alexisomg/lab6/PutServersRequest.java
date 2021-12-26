package alexisomg.lab6;

import java.util.List;

public class PutServersRequest {
    private final List<String> servers;

    public PutServersRequest(List<String> servers) {
        this.servers = servers;
    }

    public List<String> getServers() {
        return servers;
    }
}
