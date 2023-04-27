#include <iostream> 
using namespace std;

uint64_t fib_recursive(unsigned long long n){
    if(n<=1){
        return n;
    }
    return fib_recursive(n-1) + fib_recursive(n-2);
}

int main(int argc, char *argv[]){
    if(argc<2){
        throw std::invalid_argument("One parameter expected");
    }
    int n = atoi(argv[1]);
    uint64_t res = fib_recursive(n);
    cout<< res <<endl;
    return 0;
}