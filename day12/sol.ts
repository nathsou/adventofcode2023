import { readFileSync } from "fs";

const input = readFileSync('input.txt').toString();

// this is essentially picross/nonograms
type Puzzle = { row: string, hints: number[] };

const lines = input.trim().split('\n').map(s => s.trim().split(' '));
const puzzles: Puzzle[] = lines
    .map(([p, hints]) => ({ row: p, hints: hints.split(',').map(s => Number(s)) }));

const memo = new Map<string, number>();

function generateRow(offset: number, len: number, hint: number, isLastHint: boolean): string {
    if (isLastHint) {
        // only blanks after this hint
        return '.'.repeat(offset) + '#'.repeat(hint) + '.'.repeat(Math.max(len - offset - hint, 0));
    }

    return '.'.repeat(offset) + '#'.repeat(hint) + '.';
}

function countArrangements(row: string, hints: number[]): number {
    let count = 0;
    const [hint, ...remHints] = hints;
    const key = `${row}:${hints.join(',')}`;

    if (memo.has(key)) {
        return memo.get(key)!;
    }

    if (hints.length === 0) {
        count = 1;
    } else {
        for (let i = 0; i < row.length; i++) {
            // check if this position is valid for the first hint
            const head = generateRow(i, row.length, hint, remHints.length === 0);
            if (isValid(head, row.slice(0, head.length))) {
                count += countArrangements(row.slice(head.length), remHints);
            }
        }
    }

    memo.set(key, count);

    return count;
}

function isValid(arrangement: string, row: string): boolean {
    if (arrangement.length !== row.length) {
        return false;
    }

    for (let i = 0; i < row.length; i++) {
        if (row[i] !== '?' && arrangement[i] !== '?' && row[i] !== arrangement[i]) {
            return false
        }
    }

    return true;
}

function sum(ns: number[]): number {
    return ns.reduce((a, b) => a + b, 0);
}

function part1() {
    return sum(puzzles.map(p => countArrangements(p.row, p.hints)));
}

function part2() {
    const folded = puzzles.map<Puzzle>(({ row, hints }) => {
        return {
            row: [row, row, row, row, row].join('?'),
            hints: [hints, hints, hints, hints, hints].flat(),
        };
    });

    return sum(folded.map(p => countArrangements(p.row, p.hints)));
}

console.log(part1());
console.log(part2());
