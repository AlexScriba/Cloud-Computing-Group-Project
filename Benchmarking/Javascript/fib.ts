
const fib_recursive = (n: number): number => {
  if(n <= 1) return n

  return fib_recursive(n-1) + fib_recursive(n-2)
}

let args = process.argv

if(args.length < 3) throw new Error("One parameter expected");

let n = parseInt(args[2])
if(n === NaN) throw new Error("Number format Incorrect")

let res = fib_recursive(n)
console.log(res)
