use std::cmp;

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

fn main() {
    let mut s = String::new();
    let mut t = String::new();

    println!("Enter two strings: ");
    std::io::stdin().read_line(&mut s).unwrap();
    std::io::stdin().read_line(&mut t).unwrap();

    let s = s.trim();
    let t = t.trim();

    let distance = levenshtein_distance(s, t);

    println!(
        "Levenshtein distance between {} and {} is {}.",
        s, t, distance
    );
}
