import { readFileSync } from "fs";

const input = readFileSync('input.txt').toString();

type Pattern = string[];

function parsePattern(lines: string[]): Pattern {
    return lines;
}

function parseInput(input: string): Pattern[] {
    return input.split('\n\n').map(block => parsePattern(block.split('\n')));
}

const patterns = parseInput(input);

function at(pattern: Pattern, x: number, y: number): string | null {
    const row = pattern[y];

    if (!row) {
        return null;
    }

    if (x < 0 || x >= row.length) {
        return null;
    }

    return row[x];
}

function getColumn(pattern: Pattern, x: number): string | null {
    if (x < 0 || x >= pattern[0].length) {
        return null;
    }

    return pattern.map(row => row[x]).join('');
}

function isMirroredVertically(pattern: Pattern, x: number, findSmudge: boolean) {
    let foundAlmostMirroredLine = false;

    for (let i = 1; i <= x; i++) {
        const l = x - i;
        const r = x + i - 1;
        const left = getColumn(pattern, l);
        const right = getColumn(pattern, r);

        if (left == null || right == null) {
            continue;
        }

        if (left !== right) {
            const sameCount = left.split('').filter((c, i) => c === right[i]).length;
            if (findSmudge && sameCount === left.length - 1) {
                foundAlmostMirroredLine = true;
            } else {
                return false;
            }
        }
    }

    return !findSmudge || foundAlmostMirroredLine;
}

function isMirroredHorizontally(pattern: Pattern, y: number, findSmudge: boolean) {
    let foundAlmostMirroredLine = false;

    for (let i = 1; i <= y; i++) {
        const t = y - i;
        const b = y + i - 1;
        const top = pattern[t];
        const bottom = pattern[b];

        if (top == null || bottom == null) {
            continue;
        }


        if (top !== bottom) {
            const sameCount = top.split('').filter((c, i) => c === bottom[i]).length;
            if (findSmudge && sameCount === top.length - 1) {
                foundAlmostMirroredLine = true;
            } else {
                return false;
            }
        }
    }

    return !findSmudge || foundAlmostMirroredLine;
}

function sum(ns: number[]) {
    return ns.reduce((a, b) => a + b, 0);
}

function findMirrors(pattern: Pattern, findSmudge: boolean): [number[], number[]] {
    const width = pattern[0].length;
    const height = pattern.length;

    const mirrorsX: number[] = [];
    const mirrorsY: number[] = [];

    for (let x = 1; x < width; x++) {
        if (isMirroredVertically(pattern, x, findSmudge)) {
            mirrorsX.push(x);
            break;
        }
    }

    for (let y = 1; y < height; y++) {
        if (isMirroredHorizontally(pattern, y, findSmudge)) {
            mirrorsY.push(y);
            break;
        }
    }

    return [mirrorsX, mirrorsY];
}

function total(findSmudge: boolean) {
    return sum(patterns.map(p => {
        const [xs, ys] = findMirrors(p, findSmudge);
        return sum(xs) + 100 * sum(ys);
    }));
}

const part1 = () => total(false);
const part2 = () => total(true);

console.log(part1());
console.log(part2());
