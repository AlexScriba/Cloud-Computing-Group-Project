use std::{cmp, process};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::sync::{mpsc, Arc};
use std::thread;
use std::env;

fn min(x: usize, y: usize, z: usize) -> usize {
    cmp::min(x, cmp::min(y, z))
}

fn levenshtein_distance(s: &str, t: &str) -> usize {
    let m = s.len();
    let n = t.len();

    if m == 0 {
        return n;
    }

    if n == 0 {
        return m;
    }

    let mut d = vec![vec![0; n + 1]; m + 1];

    for i in 0..=m {
        d[i][0] = i;
    }

    for j in 0..=n {
        d[0][j] = j;
    }

    for j in 1..=n {
        for i in 1..=m {
            if s.chars().nth(i - 1) == t.chars().nth(j - 1) {
                d[i][j] = d[i - 1][j - 1];
            } else {
                d[i][j] = 1 + min(d[i - 1][j], d[i][j - 1], d[i - 1][j - 1]);
            }
        }
    }

    d[m][n]
}

fn compare_distances(
    words: Arc<Vec<String>>,
    start: usize,
    end: usize,
    sender: mpsc::Sender<(usize, usize, usize)>,
) {
    println!("Thread dispatched for {}-{}", start, end);

    for i in start..end {
        for j in i+1..words.len() {
            let distance = levenshtein_distance(&words[i], &words[j]);
            sender.send((i, j, distance)).unwrap();
        }
    }

    println!("Thread finished for {}-{}", start, end);
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = env::args().collect();

    let file_path = match args.get(1) {
        Some(path) => path,
        None => {
            eprintln!("Please give path to text file.");
            process::exit(1);
        }
    };

    let num_threads = match args.get(2) {
        Some(maybe_num) => match maybe_num.parse::<usize>() {
            Ok(num) => num,
            Err(r) => {
                eprintln!("Invalid format for num threads: {}", r);
                process::exit(1);
            }
        },
        None => {
            eprintln!("Expected number of threads");
            process::exit(1);
        }
    };

    // Read the input strings from the file
    let file = File::open(file_path).expect("Unable to open file");
    let reader = BufReader::new(file);
    let words: Vec<String> = reader.lines().filter_map(Result::ok).collect();
    let words = Arc::new(words);

    // Create a channel for communication betweeen the master thread and
    // worker threads
    let (sender, receiver) = mpsc::channel();

    // Calculate chunk size
    let chunk_size = (words.len() + num_threads - 1) / num_threads;

    // Dispatch worker threads
    let mut thread_handles = vec![];
    for i in 0..num_threads {
        let words = Arc::clone(&words);
        let sender = sender.clone();
        let start = i * chunk_size;
        let end = ((i + 1) * chunk_size).min(words.len());

        let handle = thread::spawn(move || {
            compare_distances(words, start, end, sender);
        });
        thread_handles.push(handle);
    }

    // Collect the results and find the smallest distance
    let mut min_distance = usize::MAX;
    let mut min_indices = (0,0);
    for _ in 0..(words.len() * (words.len() - 1) / 2) {
        let (i, j, distance) = receiver.recv().unwrap();
        if distance < min_distance {
            min_distance = distance;
            min_indices = (i, j);
        }
    }

    // Print the most similar strings
    println!(
    "Most similar string are: \n\t{} \n\t{} \nWith distance of {}",
    words[min_indices.0], words[min_indices.1], min_distance
    );

    // Wait for all threads to finish
    for handle in thread_handles {
        handle.join().unwrap();
    }
}
