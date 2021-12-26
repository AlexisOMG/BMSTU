package alexisomg.lab6;

import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.http.javadsl.ServerBinding;
import akka.stream.ActorMaterializer;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.CompletionStage;

public class ProxyApp {
    public static void main(String[] args) throws IOException {
        ActorSystem system = ActorSystem.create("lab6");
        ActorRef configActor = system.actorOf(Props.create(ConfigActor.class));
        final ActorMaterializer materializer = ActorMaterializer.create(system);
        List<CompletionStage<ServerBinding>> servers = ZooWatcher.initializeServers(configActor, system, materializer, args);

        if (servers.size() == 0) {
            System.out.println("Unable to run any server");
            System.exit(-1);
        }

        System.in.read();

        for (CompletionStage<ServerBinding> server : servers) {
            server.thenCompose(ServerBinding::unbind).thenAccept(unbinded -> system.terminate());
        }
    }
}
