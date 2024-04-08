import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.stream.Stream;

class Status {
    public long min;
    public long max;
    public long total;
    public long count;
}

public class Main {
    public final static String OUTPUTS_PATH = "outputs.txt";
    public final static String MEASUREMENTS_PATH = "measurements.txt";

    public static String solution() throws Exception {
        // List<String> lines = Files.readAllLines(Paths.get(MEASUREMENTS_PATH));

        Stream<String> lines = Files.lines(Paths.get(MEASUREMENTS_PATH));

        HashMap<String, Status> statusMap = new HashMap<>();

        lines.forEach(line -> {
            String[] splited = line.split(";");
            String cityName = splited[0];
            String measurement = splited[1];
            Long measurementNumber = Long.parseLong(measurement);

            Status status = statusMap.get(cityName);
            if (status == null) {
                status = new Status();
                status.min = measurementNumber;
                status.max = measurementNumber;
                status.total = measurementNumber;
                status.count = 1;
                statusMap.put(cityName, status);
            } else {
                status.min = Math.min(status.min, measurementNumber);
                status.max = Math.max(status.max, measurementNumber);
                status.total += measurementNumber;
                status.count++;
            }
        });

        StringBuilder builder = new StringBuilder();

        Object[] keySet = statusMap.keySet().toArray();
        Arrays.sort(keySet);

        for (Object cityNameObj : keySet) {
            String cityName = (String) cityNameObj;
            Status status = statusMap.get(cityName);
            long avg = status.total / status.count;
            builder.append(cityName).append("=").append(status.min).append(";").append(status.max).append(";")
                    .append(avg)
                    .append("(").append(status.total).append("/").append(status.count).append(")").append("\n");
        }

        return builder.toString();
    }

    public static void main(String[] args) {
        try {
            String expectOutputs = Files.readString(Paths.get(OUTPUTS_PATH));

            long startTime = System.nanoTime();
            String outputs = solution();
            long endTime = System.nanoTime();

            System.out.println("Execution Time: " + (endTime - startTime) / 1000000 + "ms");

            if (expectOutputs.equals(outputs)) {
                System.out.println("Correct!");
            } else {
                System.out.println("Incorrect!");
                System.out.println("Expect: \n" + expectOutputs);
                System.out.println("Output: \n" + outputs);
            }

            Files.writeString(Paths.get(MEASUREMENTS_PATH), "0");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}