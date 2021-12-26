package alexisomg.join;

import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

import java.io.IOException;

public class AirportMapper extends Mapper <LongWritable, Text, Key, Text> {
    @Override
    protected void map(LongWritable key, Text value, Context context) throws IOException, InterruptedException {
        if (key.get() == Constants.COLUMNS_NAMES_INDEX) {
            return;
        }
        AirportReader airport = AirportReader.read(value.toString());
        context.write(airport.getKey(), new Text(airport.getAirportName()));
    }
}
