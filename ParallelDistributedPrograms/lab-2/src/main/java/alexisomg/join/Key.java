package alexisomg.join;

import org.apache.hadoop.io.WritableComparable;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;

public class Key implements WritableComparable<Key> {
    private int code;
    private int isFlight;

    public Key(){}

    public Key(int code, int isFlight) {
        this.code = code;
        this.isFlight = isFlight;
    }

    public int getCode() {
        return this.code;
    }


    @Override
    public int compareTo(Key other) {
        if (this.code == other.code) {
            return this.isFlight - other.isFlight;
        }
        return this.code - other.code;
    }

    @Override
    public void write(DataOutput dataOutput) throws IOException {
        dataOutput.writeInt(this.code);
        dataOutput.writeInt(this.isFlight);
    }

    @Override
    public void readFields(DataInput dataInput) throws IOException {
        this.code = dataInput.readInt();
        this.isFlight = dataInput.readInt();
    }
}
