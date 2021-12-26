package alexisomg.join;

import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Reducer;

import java.io.IOException;
import java.util.Iterator;

public class JoinReducer extends Reducer<Key, Text, Text, Text> {
    @Override
    protected void reduce(Key key, Iterable<Text> values, Context context) throws IOException, InterruptedException {
        Iterator<Text> iterator = values.iterator();
        String airportName = iterator.next().toString();
        if (!iterator.hasNext()) {
            return;
        }
        float firstDelay = Float.parseFloat(iterator.next().toString());
        float max = firstDelay, min = firstDelay, sum = firstDelay;
        int cnt = 1;
        while (iterator.hasNext()) {
            float delay = Float.parseFloat(iterator.next().toString());
            max = Math.max(max, delay);
            min = Math.min(min, delay);
            sum += delay;
            ++cnt;
        }
        context.write(new Text(airportName), new Text(String.format("min: %f, max: %f, average: %f", min, max, sum/cnt)));
    }
}
