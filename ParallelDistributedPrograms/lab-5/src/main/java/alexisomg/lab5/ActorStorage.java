package alexisomg.lab5;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.japi.Pair;
import akka.japi.pf.ReceiveBuilder;

import java.util.HashMap;
import java.util.Optional;

public class ActorStorage extends AbstractActor {
    private final HashMap<Pair<String, Integer>, RequestResult> data = new HashMap<>();
    @Override
    public Receive createReceive() {
        return ReceiveBuilder.create()
                .match(
                        RequestResult.class,
                        reqRes -> data.put(new Pair<>(reqRes.getUrl(), reqRes.getRequestCnt()), reqRes)
                )
                .match(
                        GetRequestResult.class,
                        getRequestResult -> {
                            Optional<RequestResult> res = Optional.ofNullable(
                                    data.get(new Pair<>(getRequestResult.getUrl(), getRequestResult.getRequestCnt()))
                            );
                            sender().tell(res, ActorRef.noSender());
                        }
                )
                .build();
    }
}
