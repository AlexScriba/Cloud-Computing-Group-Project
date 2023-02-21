#include <iostream>
#include <vector>
#include <string>
#include <fstream>

using namespace std;

int levDist(const string& str1, const string& str2) {
	int m = str1.length(); // rows of matrix = length of str1 
	int n = str2.length(); // columns of matrix = length of str2  

	// Create a matrix to store results of subproblems (m+1)*(n+1) size  
	vector<vector<int>> dp((n+1), vector<int>(m + 1, 0));

	// Initialize first row and column as 0  
	for (int i = 1; i <= m; i++) { dp[i][0] = i; }
	for (int j = 1; j <= n; j++) { dp[0][j] = j; }

	for (int j=1; j<=n; j++) {          
		for (int i=1; i<=m ;i++) {              
			const int substitutionCost =( str1[i - 1] == str2[j - 1] ) ? 0 : 1;

			dp[i][j] = min(dp[i-1][j]+1, min(dp[i][j-1]+1,dp[i-1][j-1] + substitutionCost));
		}      
	}  

	return dp[m][n];
}

int main() {
	vector<string> strings;
	string line;
	int numlines = 0;

	ifstream reader("strings.txt");

	if (!reader)
	{
		cout << "There was an error opening file" << endl;
		return -1;
	}

	while (getline(reader, line))
	{
		
		strings.push_back(line);
		numlines++;
	}

	reader.close();

	cout << "The number of lines in this file is " << numlines << endl;

	int comparisons = 0;
	int closest = INT_MAX;
	vector<int> indeces(2);

	for (int i = 0; i < strings.size() - 1; i++) {
		for (int j = i + 1; j < strings.size(); j++) {
			const string str1 = strings[i];
			const string str2 = strings[j];

			const int dist = levDist(str1, str2);

			if (dist < closest) {
				closest = dist;
				indeces = { i,j };
			}

			comparisons++;

			if (comparisons % 1000 == 0) {
				cout << "Comps: " << comparisons << "\n";
			}
		}
	}

	cout << "Closest: " << closest << "\nIndeces: (" << indeces[0] << "," << indeces[1] << ")";

	return 0;
}