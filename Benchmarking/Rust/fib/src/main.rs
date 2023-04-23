use std::env;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    let n = match args.get(1) {
        Some(arg) => match arg.parse::<u64>() {
            Ok(number) => number,
            Err(e) => {
                eprintln!("Failed to parse the input: {}", e);
                process::exit(1);
            }
        },
        None => {
            eprintln!("Expected one argument");
            process::exit(1);
        }
    };

    // let fib = fib_iterative(n);
    let fib = fib_recursive(n);
    println!("Result: {}", fib);
}

// Might be too fast to compute
// Number gets too big in milliseocnds
fn fib_iterative(n: u64) -> u64 {
    if n <= 1 {
        return n;
    }
    let mut a = 0;
    let mut b = 1;

    for _ in 2..=n {
        let tmp = a + b;
        a = b;
        b = tmp;
    }
    b
}

fn fib_recursive(n: u64) -> u64 {
    if n <= 1 {
        return n;
    } else {
        return fib_recursive(n - 1) + fib_recursive(n - 2);
    }
}
