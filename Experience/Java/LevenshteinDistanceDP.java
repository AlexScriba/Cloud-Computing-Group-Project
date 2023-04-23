// Java implementation of Levenshtein distance calculation
// Using Dynamic Programming (Optimised solution)

import java.io.File;
import java.io.FileNotFoundException;
import java.util.*;
class LevenshteinDistanceDP {

	static int compute_Levenshtein_distanceDP(String str1,
											String str2)
	{

		// A 2-D matrix to store previously calculated
		// answers of subproblems in order
		// to obtain the final

		int[][] dp = new int[str1.length() + 1][str2.length() + 1];

		for (int i = 0; i <= str1.length(); i++)
		{
			for (int j = 0; j <= str2.length(); j++) {

				// If str1 is empty, all characters of
				// str2 are inserted into str1, which is of
				// the only possible method of conversion
				// with minimum operations.
				if (i == 0) {
					dp[i][j] = j;
				}

				// If str2 is empty, all characters of str1
				// are removed, which is the only possible
				// method of conversion with minimum
				// operations.
				else if (j == 0) {
					dp[i][j] = i;
				}

				else {
					// find the minimum among three
					// operations below

					
					dp[i][j] = minm_edits(dp[i - 1][j - 1]
						+ NumOfReplacement(str1.charAt(i - 1),str2.charAt(j - 1)), // replace
						dp[i - 1][j] + 1, // delete
						dp[i][j - 1] + 1); // insert
				}
			}
		}

		return dp[str1.length()][str2.length()];
	}

	// check for distinct characters
	// in str1 and str2

	static int NumOfReplacement(char c1, char c2)
	{
		return c1 == c2 ? 0 : 1;
	}

	// receives the count of different
	// operations performed and returns the
	// minimum value among them.

	static int minm_edits(int... nums)
	{

		return Arrays.stream(nums).min().orElse(
			Integer.MAX_VALUE);
	}

	// Driver Code
	public static void main(String args[]) throws FileNotFoundException
	{

        Scanner sc = new Scanner(new File("../strings.txt"));
        List<String> strings = new ArrayList<String>();
        while (sc.hasNextLine()) {
            strings.add(sc.nextLine());
        }


		Integer comparisons = 0;
        Integer closest = Integer.MAX_VALUE;
        int[] indeces = {-1,-1};

        for (int i = 0; i < strings.size() - 1; i++) {
            for (int j = i + 1; j < strings.size(); j++) {
                String str1 = strings.get(i);
                String str2 = strings.get(j);

                Integer dist = compute_Levenshtein_distanceDP(str1, str2);

                if (dist < closest) {
                    closest = dist;
                    indeces[0]=i;
                    indeces[1]=j;
                }

                comparisons++;

                if (comparisons % 1000 == 0) {
                    System.out.println("Comps: " + comparisons);
                }
            }
        }

		System.out.println("Closest: " + closest);
        System.out.println("Indeces: (" + indeces[0] + "," + indeces[1] + ")");
	}
}
