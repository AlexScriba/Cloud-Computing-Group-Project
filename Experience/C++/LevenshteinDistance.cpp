#include <algorithm>
#include <iostream>
#include <fstream>
#include <string.h>
#include <thread>
#include <vector>
#include <mutex>
#include <queue>

using namespace std;


int levenshtein_distance(const string& str1, const string& str2) {
	int m = str1.length(); // rows of matrix = length of str1 
	int n = str2.length(); // columns of matrix = length of str2  

	// Create a matrix to store results of subproblems (m+1)*(n+1) size  
	vector<vector<int>> dp((n + 1), vector<int>(m + 1, 0));

	// Initialize first row and column as 0  
	for (int i = 1; i <= m; i++) { dp[i][0] = i; }
	for (int j = 1; j <= n; j++) { dp[0][j] = j; }

	for (int j = 1; j <= n; j++) {
		for (int i = 1; i <= m; i++) {
			const int substitutionCost = (str1[i - 1] == str2[j - 1]) ? 0 : 1;

			dp[i][j] = min(dp[i - 1][j] + 1, min(dp[i][j - 1] + 1, dp[i - 1][j - 1] + substitutionCost));
		}
	}

	return dp[m][n];
}

void compare_distances(const vector<string>& words, size_t start, size_t end,
	queue<vector<int>>& results, mutex& mtx) {
	cout << "Thread dispatched for " << start << "-" << end << endl;

	for (size_t i = start; i < end; i++) {
		for (size_t j = i + 1; j < words.size(); j++) {
			int distance = levenshtein_distance(words[i], words[j]);
			lock_guard<mutex> lock(mtx);
			if (i != j) { 
				results.push({ (int) i, (int) j, distance }); };
		}
	}

	cout << "Thread finished for " << start << "-" << end << endl;
}
int main(int argc, char* argv[]) {
	// Get command line arguments
	if (argc < 3) {
		cerr << "Please give path to text file and number of threads" << endl;
		return 1;
	}

	string file_path(argv[1]);

	int num_threads = stoi(argv[2]);

	// Read the input strings from the file
	ifstream file(file_path);
	if (!file.is_open()) {
		cerr << "Unable to open file" << endl;
		return 1;
	}

	vector<string> words;
	string line;
	while (getline(file, line)) {
		words.push_back(line);
	}
	file.close();

	// Create a queue for communication between the master thread and worker threads
	queue<vector<int>> results;
	mutex mtx;

	// Calculate chunk size
	size_t chunk_size = (words.size() + num_threads - 1) / num_threads;

	// Dispatch worker threads
	vector<thread> thread_handles;
	for (int i = 0; i < num_threads; ++i) {
		auto start = i * chunk_size;
		auto end = min((i + 1) * chunk_size, words.size());
		auto sub_words = vector<string>(words.begin() + start, words.begin() + end);

		thread_handles.push_back(thread(compare_distances, words, start, end,
			ref(results), ref(mtx)));
	}

	// Wait for all threads to finish
	for (auto& handle : thread_handles) {
		handle.join();
	}

	// Collect the results and find the smallest distance
	int min_distance = INT32_MAX;
	vector<size_t> min_indices;
	for (size_t i = 0; i < words.size() * (words.size() - 1) / 2; ++i) {
		mtx.lock();
		if (!results.empty()) {
			auto res = results.front();
			size_t i = res[0];
			size_t j = res[1];
			size_t distance = res[2];
			results.pop();
			if (distance < min_distance) {
				min_distance = distance;
				min_indices = { i, j };
			}
		}
		mtx.unlock();
	}

	// Print the most similar strings
	cout << "Most similar string are: \n\t" << words[min_indices[0]] << "\n\t"
		<< words[min_indices[1]] << "\nWith distance of " << min_distance << endl;

	

	return 0;
}