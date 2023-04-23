package Benchmarking.Java;

public class FibRecursive {
    public static long fibRecursive(long n) {
        if (n <= 1) {
            return n;
        }
        return fibRecursive(n - 1) + fibRecursive(n - 2);
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            throw new IllegalArgumentException("One parameter expected");
        }
        long n = Long.parseLong(args[1]);
        long res = fibRecursive(n);
        System.out.println(res);
    }
}
