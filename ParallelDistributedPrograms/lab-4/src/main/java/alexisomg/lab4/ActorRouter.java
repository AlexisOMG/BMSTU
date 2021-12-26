package alexisomg.lab4;

import akka.actor.AbstractActor;
import akka.actor.ActorRef;
import akka.actor.Props;
import akka.routing.*;

import java.util.ArrayList;
import java.util.List;

public class ActorRouter extends AbstractActor {
    private final ActorRef storage;
    private final Router router;

    public ActorRouter(){
        storage = getContext().actorOf(Props.create(ActorStorage.class));
        List<Routee> routees = new ArrayList<>();
        for (int i = 0; i < 10; ++i) {
            ActorRef ref = getContext().actorOf(Props.create(ActorExecutor.class));
            getContext().watch(ref);
            routees.add(new ActorRefRoutee(ref));
        }
        router = new Router(new RoundRobinRoutingLogic(), routees);
    }



    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(
                    PostTestRequest.class,
                    message -> {
                        String packageID = message.getPackageID();
                        String script = message.getScript();
                        String name = message.getFuncName();

                        for (TestBody t : message.getTestBodies()) {
                            router.route(
                                new PackageTest(packageID, script, name, t), storage
                            );
                        }
                    }
                )
                .match(
                    GetTestRequest.class,
                    message -> storage.tell(message, sender())
                )
                .build();
    }
}
