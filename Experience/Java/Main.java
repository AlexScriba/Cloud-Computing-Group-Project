import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Main {
	
	public static int levenshteinDistance(String str1, String str2) {
        int m = str1.length();
        int n = str2.length();
        int[][] dp = new int[m + 1][n + 1];

        for (int i = 1; i <= m; i++) {
            dp[i][0] = i;
        }

        for (int j = 1; j <= n; j++) {
            dp[0][j] = j;
        }

        for (int j = 1; j <= n; j++) {
            for (int i = 1; i <= m; i++) {
                int substitutionCost = str1.charAt(i - 1) == str2.charAt(j - 1) ? 0 : 1;

                dp[i][j] = Math.min(dp[i - 1][j] + 1,
                    Math.min(dp[i][j - 1] + 1, dp[i - 1][j - 1] + substitutionCost));
            }
        }

        return dp[m][n];
    }

    public static void compareDistances(List<String> words, int start, int end,
            Queue<int[]> results, Lock mtx) {
        System.out.println("Thread dispatched for " + start + "-" + end);

        for (int i = start; i < end; i++) {
            for (int j = i + 1; j < words.size(); j++) {
                int distance = levenshteinDistance(words.get(i), words.get(j));
                mtx.lock();
                if (i != j) {
                    results.add(new int[] { i, j, distance });
                }
                mtx.unlock();
            }
        }

        System.out.println("Thread finished for " + start + "-" + end);
    }
	
    public static void main(String[] args) {
        // Get command line arguments
        if (args.length < 2) {
            System.err.println("Please give path to text file and number of threads");
            System.exit(1);
        }

        String filePath = args[0];
        int numThreads = Integer.parseInt(args[1]);

        // Read the input strings from the file
        List<String> words = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String line;
            while ((line = br.readLine()) != null) {
                words.add(line);
            }
        } catch (IOException e) {
            System.err.println("Unable to open file");
            System.exit(1);
        }

        // Create a queue for communication between the master thread and worker threads
        Queue<int[]> results = new ConcurrentLinkedQueue<>();
        Lock mtx = new ReentrantLock();

        // Calculate chunk size
        int chunkSize = (int) Math.ceil((double) words.size()  / numThreads);

        // Dispatch worker threads
        List<Thread> threadHandles = new ArrayList<>();
        for (int i = 0; i < numThreads; ++i) {
            int start = i * chunkSize;
            int end = Math.min((i + 1) * chunkSize, words.size());

            threadHandles.add(new Thread(() -> compareDistances(words, start, end, results, mtx)));
            threadHandles.get(i).start();
        }

        // Wait for all threads to finish
        for (Thread handle : threadHandles) {
            try {
                handle.join();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }

        // Collect the results and find the smallest distance
        int minDistance = Integer.MAX_VALUE;
        int[] minIndices = new int[2];
        for (int i = 0; i < words.size() * (words.size() - 1) / 2; ++i) {
            mtx.lock();
            if (!results.isEmpty()) {
                int[] res = results.poll();
                int iVal = res[0];
                int jVal = res[1];
                int distance = res[2];
                if (distance < minDistance) {
                    minDistance = distance;
                    minIndices[0] = iVal;
                    minIndices[1] = jVal;
                }
            }
            mtx.unlock();
        }

        // Print the most similar strings
        System.out.println("Most similar string are: \n\t" + words.get(minIndices[0]) + "\n\t"
                + words.get(minIndices[1]) + "\nWith distance of " + minDistance);
    }
}