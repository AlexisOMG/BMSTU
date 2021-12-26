package alexisomg.lab5;

import akka.NotUsed;
import akka.actor.ActorRef;
import akka.actor.ActorSystem;
import akka.actor.Props;
import akka.http.javadsl.ConnectHttp;
import akka.http.javadsl.Http;
import akka.http.javadsl.ServerBinding;
import akka.http.javadsl.model.HttpRequest;
import akka.http.javadsl.model.HttpResponse;
import akka.http.javadsl.model.Query;
import akka.japi.Pair;
import akka.pattern.Patterns;
import akka.stream.ActorMaterializer;
import akka.stream.javadsl.Flow;
import akka.stream.javadsl.Keep;
import akka.stream.javadsl.Sink;
import akka.stream.javadsl.Source;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.Dsl;
import org.asynchttpclient.Request;

import java.io.IOException;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;

public class AverageApp {
    private static final String HOST = "localhost";
    private static final int PORT = 8080;
    private static final String URL_PARAM_NAME = "testUrl";
    private static final String COUNT_PARAM_NAME = "count";
    private static final int CNT_PARALLELISM = 5;
    private static final int TIMEOUT_MILLIS = 5000;

    public static void main(String[] args) throws IOException {
        System.out.println("start!");
        ActorSystem system = ActorSystem.create("routes");
        final ActorRef storage = system.actorOf(Props.create(ActorStorage.class));
        final Http http = Http.get(system);
        final ActorMaterializer materializer = ActorMaterializer.create(system);
        final Flow<HttpRequest, HttpResponse, NotUsed> routeFlow = createFlow(storage, materializer);
        final CompletionStage<ServerBinding> binding = http.bindAndHandle(
                routeFlow,
                ConnectHttp.toHost(HOST, PORT),
                materializer
        );
        System.out.println("Server online at http://localhost:8080/\n");
        System.in.read();
        binding
                .thenCompose(ServerBinding::unbind)
                .thenAccept(unbound -> system.terminate());
    }

    private static Flow<HttpRequest, HttpResponse, NotUsed> createFlow(ActorRef storage, ActorMaterializer materializer) {
        return Flow.of(HttpRequest.class)
                .map(req -> {
                    Query query = req.getUri().query();
                    String url = query.getOrElse(URL_PARAM_NAME, "");
                    int reqCnt = query.get(COUNT_PARAM_NAME).map(Integer::parseInt).orElse(1);
                    System.out.format("Url: %s, cnt: %d\n", url, reqCnt);
                    return new Pair<>(url, reqCnt);
                })
                .mapAsync(CNT_PARALLELISM, req -> {
                    GetRequestResult request = new GetRequestResult(req.first(), req.second());
                    return Patterns.ask(storage, request, Duration.ofMillis(TIMEOUT_MILLIS)).thenCompose(
                            resp -> {
                                Optional<RequestResult> response = (Optional<RequestResult>) resp;
                                if (response.isPresent()) {
                                    RequestResult result = response.get();
                                    int reqCnt = result.getRequestCnt();
                                    long time = result.getRequestTime();
                                    System.out.format("Cached time: %d\n", time/reqCnt);
                                    return CompletableFuture.completedFuture(time/reqCnt);
                                }

                                Sink<Pair<String, Integer>, CompletionStage<Long>> testSink = createTestSink(storage);
                                return Source.from(Collections.singletonList(req))
                                        .toMat(testSink, Keep.right())
                                        .run(materializer);
                            }
                    );
                })
                .map(param -> HttpResponse.create().withEntity(param.toString()));
    }

    private static Sink<Pair<String, Integer>, CompletionStage<Long>> createTestSink(ActorRef storage) {
        return Flow.<Pair<String, Integer>>create()
                .mapConcat(
                        param -> new ArrayList<>(Collections.nCopies(param.second(), param))
                )
                .mapAsync(CNT_PARALLELISM, param -> {
                    AsyncHttpClient client = Dsl.asyncHttpClient();
                    Request request = Dsl.get(param.first()).build();
                    long start = System.currentTimeMillis();
                    return client.executeRequest(request).toCompletableFuture().thenCompose(
                            response -> {
                                long currentTime = System.currentTimeMillis();
                                System.out.format("Request time: %d\n", currentTime - start);
                                return CompletableFuture.completedFuture(new RequestResult(
                                        param.first(),
                                        1,
                                        currentTime - start
                                ));
                            }
                    );
                })
                .fold(new RequestResult("", 0, 0), RequestResult::addRequestResult)
                .map(param -> {
                    storage.tell(param, ActorRef.noSender());
                    System.out.format("Average time: %d\n", param.getRequestTime() / param.getRequestCnt());
                    return param.getRequestTime() / param.getRequestCnt();
                })
                .toMat(Sink.head(), Keep.right());
    }
}
