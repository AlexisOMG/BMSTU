package alexisomg.lab4;

import akka.actor.AbstractActor;

import java.util.*;

public class ActorStorage extends AbstractActor {
    private final Map<String, List<TestingResult>> testingResults = new HashMap<>();

    public void saveResult(PackageTestResult testResult) {
        String packageID = testResult.getPackageID();
        if (testingResults.containsKey(packageID)) {
            testingResults.get(packageID).add(testResult.getResult());
        } else {
            testingResults.put(packageID, new ArrayList<>(Collections.singleton(testResult.getResult())));
        }
    }

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(
                        PackageTestResult.class,
                        this::saveResult
                )
                .match(
                        GetTestRequest.class,
                        req -> sender().tell(
                                new GetTestResponse(req.getPackageID(), testingResults.get(req.getPackageID())),
                                self()
                        )
                )
                .build();
    }
}
