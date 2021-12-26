package alexisomg.join;

import org.apache.hadoop.io.WritableComparable;
import org.apache.hadoop.io.WritableComparator;

public class Comparator extends WritableComparator {
    public Comparator() {
        super(Key.class, true);
    }

    @Override
    public int compare(WritableComparable a, WritableComparable b) {
        Key first = (Key)a;
        Key second = (Key)b;
        return first.getCode() - second.getCode();
    }
}
