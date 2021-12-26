package alexisomg.lab6;

import akka.NotUsed;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.http.javadsl.ConnectHttp;
import akka.http.javadsl.Http;
import akka.http.javadsl.ServerBinding;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.model.HttpResponse;
import akka.stream.ActorMaterializer;
import akka.stream.javadsl.Flow;
import org.apache.zookeeper.KeeperException;
import org.apache.zookeeper.WatchedEvent;
import org.apache.zookeeper.Watcher;
import org.apache.zookeeper.ZooKeeper;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletionStage;

public class ZooWatcher implements Watcher {
    public final static String ROOT_PATH = "/servers";
    private final static String ZOO_URL = "localhost:2181";
    private final static int TIMEOUT = 5000;
    private final static String SERVERS_HOST = "localhost";

    private final ActorRef configActor;
    private final ZooKeeper zooKeeper;

    public ZooWatcher(ActorRef configActor, ZooKeeper zooKeeper) throws InterruptedException, KeeperException {
        this.configActor = configActor;
        this.zooKeeper = zooKeeper;

        this.zooKeeper.getData(ROOT_PATH, true, null);
        saveServersToConfig();
    }

    private void saveServersToConfig() throws InterruptedException, KeeperException {
        List<String> servers = new ArrayList<>();
        for (String server : this.zooKeeper.getChildren(ROOT_PATH, this)) {
            servers.add(new String(zooKeeper.getData(ROOT_PATH +"/"+server, false, null)));
        }
        configActor.tell(new PutServersRequest(servers), ActorRef.noSender());
    }

    public static List<CompletionStage<ServerBinding>> initializeServers(
            ActorRef configActor, ActorSystem system, ActorMaterializer materializer, String[] args) {
        ZooKeeper zooKeeper = null;
        try {
            zooKeeper = new ZooKeeper(ZOO_URL, TIMEOUT, null);
            new ZooWatcher(configActor, zooKeeper);
        } catch (IOException | InterruptedException | KeeperException e) {
            e.printStackTrace();
        }
        final Http http = Http.get(system);

        List<CompletionStage<ServerBinding>> servers = new ArrayList<>();
        for (String port : args) {
            try {
                Server server = new Server(http, configActor, zooKeeper, port);
                final Flow<HttpRequest, HttpResponse, NotUsed> flow = server.createRoute().flow(system, materializer);
                servers.add(http.bindAndHandle(
                        flow,
                        ConnectHttp.toHost(SERVERS_HOST, Integer.parseInt(port)),
                        materializer
                ));
            } catch (InterruptedException | KeeperException e) {
                e.printStackTrace();
            }
        }

        return servers;
    }

    @Override
    public void process(WatchedEvent watchedEvent) {
        try {
            saveServersToConfig();
        } catch (InterruptedException | KeeperException e) {
            e.printStackTrace();
        }
    }
}
