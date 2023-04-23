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
Object.defineProperty(exports, "__esModule", { value: true });
const fs = __importStar(require("fs"));
const levDist = (str1, str2) => {
    const m = str1.length;
    const n = str2.length;
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
const main = () => {
    const strings = fs.readFileSync('../strings.txt').toString().split('\n');
    let comparisons = 0;
    let closest = Infinity;
    let indeces = [-1, -1];
    for (let i = 0; i < strings.length - 1; i++) {
        for (let j = i + 1; j < strings.length; j++) {
            const str1 = strings[i];
            const str2 = strings[2];
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
    console.log(`Closest: ${closest}\n Indeces: (${indeces[0]}, ${indeces[1]})`);
};
main();
