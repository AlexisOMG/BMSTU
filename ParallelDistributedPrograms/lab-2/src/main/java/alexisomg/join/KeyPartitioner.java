package alexisomg.join;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Partitioner;

public class KeyPartitioner extends Partitioner <Key, Text> {

    @Override
    public int getPartition(Key key, Text value, int cntPartitions) {
        return key.getCode() % cntPartitions;
    }
}
