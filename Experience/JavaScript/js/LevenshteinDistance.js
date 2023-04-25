"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const fs = __importStar(require("fs"));
const levDist = (str1, str2) => {
    const m = str1.length;
    const n = str2.length;
    if (m == 0) {
        return m;
    }
    if (n == 0) {
        return m;
    }
    const d = [];
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
            d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + substitutionCost);
        }
    }
    return d[m][n];
};
const compare_distances = (words, start, end) => {
    console.log(`Thread dispatched for ${start}-${end}`);
    let res = [];
    for (let i = start; i < end; i++) {
        for (let j = i + 1; j < words.length; j++) {
            let distance = levDist(words[i], words[j]);
            res.push({ i, j, distance });
        }
    }
    console.log(`Thread finished for ${start}-${end}`);
    return res;
};
const main = () => __awaiter(void 0, void 0, void 0, function* () {
    const args = process.argv;
    if (args.length < 4)
        throw new Error("Please provide arguments: <file_path> <num_threads>");
    let file_path = args[2];
    let num_threads = parseInt(args[3]);
    const strings = fs
        .readFileSync(file_path)
        .toString()
        .split("\n")
        .filter((str) => str !== ""); // filter out any falsy values (like empty line)
    let chunk_size = Math.floor((strings.length + num_threads - 1) / num_threads);
    let promises = [];
    for (let i = 0; i < num_threads; i++) {
        promises.push(new Promise((resolve, _) => {
            let start = i * chunk_size;
            let end = Math.min((i + 1) * chunk_size, strings.length);
            let distances = compare_distances(strings, start, end);
            resolve(distances);
        }));
    }
    let responses = yield Promise.all(promises);
    let comparisons = responses.flat();
    let bestRes = comparisons.reduce((prev, curr) => curr.distance < prev.distance ? curr : prev);
    console.log(`Most similar strings are: \n\t${strings[bestRes.i]}\n\t${strings[bestRes.j]}\nWith distance of ${bestRes.distance}`);
});
main();
