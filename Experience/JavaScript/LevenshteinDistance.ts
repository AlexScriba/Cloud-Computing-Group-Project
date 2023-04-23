import * as fs from 'fs';

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
            const substitutionCost = str1.charAt(i - 1) === str2.charAt(j - 1) ? 0 : 1;

            d[i][j] = Math.min(
                d[i - 1][j] + 1,
                d[i][j - 1] + 1,
                d[i - 1][j - 1] + substitutionCost
            );
        }
    }

    return d[m][n];
};

// TODO :: seems to have some bugs in output
const main = () => {
    const strings = fs.readFileSync('../strings.txt').toString().split('\n');

    let comparisons = 0;

    let closest = Infinity;
    let indeces: [number, number] = [-1, -1];

    for (let i = 0; i < strings.length - 1; i++) {
        for (let j = i + 1; j < strings.length; j++) {
            const str1 = strings[i];
            const str2 = strings[j];

            const dist = levDist(str1, str2);

            if (dist < closest) {
                closest = dist;
                indeces = [i, j];
            }

            comparisons++;

            if (comparisons % 1000 === 0) {
                console.log('Comps:', comparisons);
            }
        }
    }

    console.log(`Closest: ${closest}\nIndeces: (${indeces[0]}, ${indeces[1]})`);
};

main();

