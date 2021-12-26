package alexisomg.lab6;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class ConfigActor extends AbstractActor {
    private List<String> servers = new ArrayList<>();
    private final Random rand = new Random();

    private String getServer() {
        return servers.get(rand.nextInt(servers.size()));
    }

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(
                        GetServerRequest.class,
                        msg -> sender().tell(getServer(), ActorRef.noSender())
                )
                .match(
                        PutServersRequest.class,
                        msg -> this.servers = msg.getServers()
                )
                .build();
    }
}
