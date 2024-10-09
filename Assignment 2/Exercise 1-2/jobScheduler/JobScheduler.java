package jobScheduler;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import java.util.ArrayList;
import java.util.HashMap;

public abstract class JobScheduler <K, V> {

    public JobScheduler() {}

    // Frozen spot: Method to run the job scheduler
    public final void run() {
        Stream<AJob<K, V>> inputStream = emit();
        Stream<Pair<K, V>> execStream = compute(inputStream);
        Stream<Pair<K, List<V>>> groupedStream = collect(execStream);
        output(groupedStream);
    }

    // Hot spot: Method to generate the stream of jobs
    protected abstract Stream<AJob<K, V>> emit();

    // Frozen spot: Method to execute the jobs
    private Stream<Pair<K, V>> compute(Stream<AJob<K, V>> jobs) {
        ArrayList<Pair<K, V>> streamList = new ArrayList<>();
        jobs.forEach(j -> {
            Stream<Pair<K, V>> l = j.execute();
            l.forEach(item -> {
                streamList.add(item);
            });
        });
        return streamList.stream();

    }

    // Frozen spot: Method to group the results
    public Stream<Pair<K, List<V>>> collect(Stream<Pair<K, V>> pairs){
        Map<K, List<V>> map = new HashMap<>();
        pairs.forEach(pair -> {
            if (!map.containsKey(pair.getKey())) {
                map.put(pair.getKey(), new ArrayList<>());
            }
            map.get(pair.getKey()).add(pair.getValue());
        });
        return map.entrySet().stream().map(e -> new Pair<>(e.getKey(), e.getValue()));
    }

    // Hot spot: Method to output the results
    protected abstract void output(Stream<Pair<K, List<V>>> results);
}