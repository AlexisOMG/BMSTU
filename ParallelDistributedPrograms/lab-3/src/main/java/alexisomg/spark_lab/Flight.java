package alexisomg.spark_lab;

import java.io.Serializable;

public class Flight implements Serializable {
    private final float delay;
    private final boolean cancelled;

    public Flight(float delay, boolean cancelled) {
        this.delay = delay;
        this.cancelled = cancelled;
    }

    public float getDelay() {
        return this.delay;
    }

    public boolean wasCancelled() {
        return this.cancelled;
    }
}
