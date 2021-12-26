package alexisomg.lab4;

import akka.NotUsed;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.http.javadsl.ConnectHttp;
import akka.http.javadsl.Http;
import akka.http.javadsl.marshallers.jackson.Jackson;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.model.HttpResponse;
import akka.http.javadsl.server.AllDirectives;
import akka.http.javadsl.server.Route;
import akka.pattern.Patterns;
import akka.stream.ActorMaterializer;
import akka.stream.javadsl.Flow;
import scala.concurrent.Future;


import java.io.IOException;

public class TestApp extends AllDirectives{
    private static final String HOST = "localhost";
    private static final int PORT = 8080;
    private static final String GET_PARAMETER = "packageId";
    public static void main(String[] args) throws IOException{
        ActorSystem actorSystem = ActorSystem.create("TestApp");
        ActorRef router = actorSystem.actorOf(Props.create(ActorRouter.class));

        final Http http = Http.get(actorSystem);

        final ActorMaterializer materializer = ActorMaterializer.create(actorSystem);

        TestApp inst = new TestApp();

        final Flow<HttpRequest, HttpResponse, NotUsed> flow = inst.serve(router).flow(actorSystem, materializer);


        http.bindAndHandle(flow, ConnectHttp.toHost(HOST, PORT), materializer);
    }

    public Route serve(ActorRef router) {
        return concat(
                path("test", () -> route(
                        get(()->parameter(GET_PARAMETER, packageId -> {
                            GetTestRequest msg = new GetTestRequest(packageId);
                            Future<Object> res = Patterns.ask(router, msg, 5000);
                            return completeOKWithFuture(res, Jackson.marshaller());
                        })),
                        post(() ->
                            entity(Jackson.unmarshaller(PostTestRequest.class), msg -> {
                                router.tell(msg, ActorRef.noSender());
                                return complete("Lets go");
                            })
                        )
                ))
        );
    }
}
