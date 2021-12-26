package alexisomg.join;

import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.lib.input.TextInputFormat;
import org.apache.hadoop.mapreduce.lib.input.MultipleInputs;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class JoinApp {
    public static void main(String[] args) throws Exception {
        if (args.length != Constants.ARGS_LENGTH) {
            System.err.println(Constants.USAGE_MESSAGE);
            System.exit(-1);
        }

        Job job = Job.getInstance();
        job.setJarByClass(JoinApp.class);
        job.setJobName("Join (reduce-side)");

        MultipleInputs.addInputPath(job, new Path(args[Constants.AIRPORTS_INPUT_PATH_POS]), TextInputFormat.class, AirportMapper.class);
        MultipleInputs.addInputPath(job, new Path(args[Constants.FLIGHTS_INPUT_PATH_POS]), TextInputFormat.class, FlightMapper.class);

        job.setGroupingComparatorClass(Comparator.class);
        job.setReducerClass(JoinReducer.class);
        job.setPartitionerClass(KeyPartitioner.class);

        FileOutputFormat.setOutputPath(job, new Path(args[Constants.OUTPUT_PATH_POS]));

        job.setMapOutputKeyClass(Key.class);
        job.setOutputValueClass(Text.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        job.setNumReduceTasks(Constants.NUM_REDUCE_TASKS);

        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
