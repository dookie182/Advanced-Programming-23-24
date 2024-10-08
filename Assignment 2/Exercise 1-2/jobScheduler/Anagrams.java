package jobScheduler;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Stream;

public class Anagrams extends JobScheduler<String, String> {

    public Anagrams() {}

    protected void output(Stream<Pair<String, List<String>>> results) {
        File outputFile = new File("count_anagrams.txt");
        if (!outputFile.exists()) {
            outputFile = new File("count_anagrams.txt");
        }
        
        try (FileWriter writer = new FileWriter(outputFile.getAbsolutePath(), true)) {
            results.forEach(pair -> {
                try {
                    writer.write(pair.getKey() + " " + pair.getValue().size() + "\n");
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    protected Stream <AJob<String, String>> emit() {
        Scanner sc = new Scanner(System.in);
        System.out.println("Enter the directory path: ");
        String dirPath = sc.nextLine();
        sc.close();
        //Creating a File object for directory
        File directoryPath = new File(dirPath);
        //List of all files and directories
        File filesList[] = directoryPath.listFiles();
        List<AJob<String, String>> jobs = new ArrayList<>();
        for (File file : filesList) {
            jobs.add(new Task(file.getAbsolutePath()));
        }
        return jobs.stream();
    }

    public static void main(String[] args) {
        Anagrams anagramCounter = new Anagrams();
        anagramCounter.run();
    }


    

}
