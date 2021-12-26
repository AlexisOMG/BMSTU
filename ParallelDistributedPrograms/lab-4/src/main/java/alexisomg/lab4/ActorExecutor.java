package alexisomg.lab4;

import akka.actor.AbstractActor;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

public class ActorExecutor extends AbstractActor {
    private final String STATUS_OK = "OK";
    private final String STATUS_FAIL = "FAIL";
    private final String STATUS_ERROR = "ERROR";

    @Override
    public Receive createReceive() {
        return receiveBuilder()
                .match(
                        PackageTest.class,
                        message -> sender().tell(
                                test(message),
                                self()
                        )
                )
                .build();
    }

    public String execute(String script, String funcName, Object[] args) throws ScriptException, NoSuchMethodException {
        ScriptEngine engine = new ScriptEngineManager().getEngineByName("nashorn");
        engine.eval(script);
        Invocable invocable = (Invocable) engine;
        return invocable.invokeFunction(funcName, args).toString();
    }

    public PackageTestResult test(PackageTest message) {
        String actualRes = "";
        String status = "";
        String expectedRes = message.getTestBody().getExpectedRes();

        try {
            actualRes = execute(
                    message.getScript(),
                    message.getFuncName(),
                    message.getTestBody().getArgs()
            );
            status = (expectedRes.equals(actualRes) ? STATUS_OK : STATUS_FAIL);
        } catch (ScriptException | NoSuchMethodException err) {
            status = STATUS_ERROR;
        }

        return new PackageTestResult(
                message.getPackageID(),
                status,
                message.getFuncName(),
                message.getTestBody().getExpectedRes(),
                actualRes
        );
    }
}
