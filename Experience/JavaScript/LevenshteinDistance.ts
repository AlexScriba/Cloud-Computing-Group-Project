import * as fs from "fs";

const levDist = (str1: string, str2: string) => {
  const m = str1.length;
  const n = str2.length;

  if (m == 0) {
    return m;
  }

  if (n == 0) {
    return m;
  }

  const d: number[][] = [];

  for (let i = 0; i <= m; i++) {
    let row = new Array(n + 1).fill(0);
    d.push(row);
  }

  for (let i = 1; i <= m; i++) {
    d[i][0] = i;
  }

  for (let j = 1; j <= n; j++) {
    d[0][j] = j;
  }

  for (let j = 1; j <= n; j++) {
    for (let i = 1; i <= m; i++) {
      const substitutionCost =
        str1.charAt(i - 1) === str2.charAt(j - 1) ? 0 : 1;

      d[i][j] = Math.min(
        d[i - 1][j] + 1,
        d[i][j - 1] + 1,
        d[i - 1][j - 1] + substitutionCost
      );
    }
  }

  return d[m][n];
};

interface CompResult {
  i: number;
  j: number;
  distance: number;
}

const compare_distances = (
  words: string[],
  start: number,
  end: number
): CompResult[] => {
  console.log(`Thread dispatched for ${start}-${end}`);

  let res: CompResult[] = [];
  for (let i = start; i < end; i++) {
    for (let j = i + 1; j < words.length; j++) {
      let distance = levDist(words[i], words[j]);
      res.push({ i, j, distance });
    }
  }

  console.log(`Thread finished for ${start}-${end}`);

  return res;
};

const main = async () => {
  const args = process.argv;

  if (args.length < 4)
    throw new Error("Please provide arguments: <file_path> <num_threads>");

  let file_path = args[2];
  let num_threads = parseInt(args[3]);

  const strings = fs
    .readFileSync(file_path)
    .toString()
    .split("\n")
    .filter((str) => str !== ""); // filter out empty lines

  let chunk_size = Math.floor((strings.length + num_threads - 1) / num_threads);

  // TS does not have threads
  //  -> therefore using promises to mimick non blocking effect
  let promises: Promise<CompResult[]>[] = [];
  for (let i = 0; i < num_threads; i++) {
    promises.push(
      new Promise((resolve, _) => {
        let start = i * chunk_size;
        let end = Math.min((i + 1) * chunk_size, strings.length);

        let distances = compare_distances(strings, start, end);
        resolve(distances);
      })
    );
  }

  let responses = await Promise.all(promises);
  let comparisons: CompResult[] = responses.flat();

  let bestRes = comparisons.reduce((prev, curr) =>
    curr.distance < prev.distance ? curr : prev
  );

  console.log(
    `Most similar strings are: \n\t${strings[bestRes.i]}\n\t${
      strings[bestRes.j]
    }\nWith distance of ${bestRes.distance}`
  );
};

main();
