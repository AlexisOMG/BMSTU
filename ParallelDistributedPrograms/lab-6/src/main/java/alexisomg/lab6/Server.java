package alexisomg.lab6;

import akka.actor.ActorRef;
import akka.http.javadsl.Http;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.server.AllDirectives;
import akka.http.javadsl.server.Route;
import akka.pattern.Patterns;
import org.apache.zookeeper.*;

import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.Objects;

public class Server extends AllDirectives implements Watcher {
    private final static String SERVER_URL_PREFIX = "localhost:";
    private final static String SERVER_PATH_PREFIX = ZooWatcher.ROOT_PATH + "/" + SERVER_URL_PREFIX;
    private final static String URL_PARAM = "url";
    private final static String COUNT_PARAM = "count";
    private final static String ZERO_COUNT = "0";
    private final static String URL_FORMAT_PATTERN = "http://%s/?url=%s&count=%d";
    private final static long TIMEOUT = 5000;

    private final Http http;
    private final ActorRef configActor;
    private final ZooKeeper zooKeeper;
    private final String port;

    public Server(Http http, ActorRef configActor, ZooKeeper zooKeeper, String port) throws InterruptedException, KeeperException {
        this.http = http;
        this.configActor = configActor;
        this.zooKeeper = zooKeeper;
        this.port = port;
        createServer();
    }

    private void createServer() throws InterruptedException, KeeperException {
        this.zooKeeper.create(
                SERVER_PATH_PREFIX+this.port,
                (SERVER_URL_PREFIX + this.port).getBytes(StandardCharsets.UTF_8),
                ZooDefs.Ids.OPEN_ACL_UNSAFE,
                CreateMode.EPHEMERAL_SEQUENTIAL
        );
    }

    public Route createRoute() {
        return route(
                path("", () -> route(
                        get(() -> parameter(URL_PARAM, (url) ->
                                parameter(COUNT_PARAM, (count) -> {
                                    System.out.printf("URL: %s, COUNT: %S\n", url, count);
                                    if (Objects.equals(count, ZERO_COUNT)) {
                                        return completeWithFuture(this.http.singleRequest(HttpRequest.create(url)));
                                    }
                                    return completeWithFuture(
                                            Patterns
                                                    .ask(
                                                            this.configActor,
                                                            new GetServerRequest(),
                                                            Duration.ofMillis(TIMEOUT)
                                                    )
                                                    .thenCompose(res -> {
                                                        System.out.printf(
                                                                URL_FORMAT_PATTERN+"\n",
                                                                res,
                                                                url,
                                                                Integer.parseInt(count)-1
                                                        );
                                                        return this.http.singleRequest(
                                                                HttpRequest.create(
                                                                        String.format(
                                                                                URL_FORMAT_PATTERN,
                                                                                res,
                                                                                url,
                                                                                Integer.parseInt(count)-1
                                                                        )
                                                                )
                                                        );
                                                    }
                                                    )
                                    );
                                })))
                ))
        );
    }

    @Override
    public void process(WatchedEvent watchedEvent) {
        try {
            this.zooKeeper.getData(SERVER_PATH_PREFIX+this.port, this, null);
        } catch (InterruptedException | KeeperException e) {
            e.printStackTrace();
        }
    }
}
