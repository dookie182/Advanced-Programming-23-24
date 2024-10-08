package jobScheduler;
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.stream.Stream;

public class Task extends AJob<String,String> {
    private String filename = new String();
    
    public Task(String file) {
        this.filename = file;
    }

    private String ciao(String s) {
        String ciao = s.toLowerCase();
        char[] ciaoArray = ciao.toCharArray();
        Arrays.sort(ciaoArray);
        String ret = new String(ciaoArray);
        return ret;
    }

    public Stream<Pair<String, String>> execute() {
        Map<String, String> map = new HashMap<>();
        try {
            File myObj = new File(this.filename);
            Scanner myReader = new Scanner(myObj);
            while (myReader.hasNextLine()) {
                String data = myReader.nextLine();
                String[] words = data.split(" ");
                for (String word : words) {
                    word = word.replaceAll("[^a-zA-Z0-9]", "");
                    word = word.toLowerCase();
                    if (word.length() > 3) {
                        map.put(ciao(word), word);
                    }
                }
            }
            myReader.close();
        } catch (FileNotFoundException e) {
            System.out.println("An error occurred.");
            e.printStackTrace();
        }
        return map.entrySet().stream()
                  .map(entry -> new Pair<>(entry.getKey(), entry.getValue()));
    }
}
