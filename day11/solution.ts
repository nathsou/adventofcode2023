import { readFileSync } from "fs";
import { Graph, dijkstra } from "./graphs";

const input = readFileSync('input.txt', 'utf8');

function parseInput(input: string): boolean[][] {
    const universe = input.split('\n').map((line) => line.split('').map(c => c === '#'));
    return universe;
}

function expandedRowsAndCols(universe: boolean[][]): { rows: Set<number>, cols: Set<number> } {
    const emptyRows = new Set<number>();
    const emptyCols = new Set<number>();

    for (let i = 0; i < universe.length; i++) {
        if (universe[i].every(c => !c)) {
            emptyRows.add(i);
        }
    }

    for (let i = 0; i < universe[0].length; i++) {
        if (universe.every(row => !row[i])) {
            emptyCols.add(i);
        }
    }

    return { rows: emptyRows, cols: emptyCols };
}

function buildGraph(universe: boolean[][], expansionFactor: number): Graph<string> {
    const g = new Graph<string>();
    const expanded = expandedRowsAndCols(universe);

    // add edges between every adjacent cell
    const width = universe[0].length;
    const height = universe.length;

    function cost(x: number, y: number, dir: 'x' | 'y'): number {
        if (
            (dir === 'y' && expanded.rows.has(y)) ||
            (dir === 'x' && expanded.cols.has(x))
        ) {
            return expansionFactor;
        }

        return 1;
    }

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y ++) {
            const cell = `${x}:${y}`;
            g.insertVertex(cell);

            if (x > 0) {
                g.insertUndirectedEdge(cell, `${x - 1}:${y}`, cost(x - 1, y, 'x'));
            }

            if (x < width) {
                g.insertUndirectedEdge(cell, `${x + 1}:${y}`, cost(x + 1, y, 'x'));
            }

            if (y > 0) {
                g.insertUndirectedEdge(cell, `${x}:${y - 1}`, cost(x, y - 1, 'y'));
            }

            if (y < height) {
                g.insertUndirectedEdge(cell, `${x}:${y + 1}`, cost(x, y + 1, 'y'));
            }
        }
    }

    return g;
}

function findGalaxies(universe: boolean[][]): string[] {
    const gals: string[] = [];

    for (let x = 0; x < universe[0].length; x++) {
        for (let y = 0; y < universe.length; y ++) {
            if (universe[y][x]) {
                gals.push(`${x}:${y}`);
            }
        }
    }

    return gals;
}

function getPairs(galaxies: string[]): [string, string][] {
    const pairs: [string, string][] = [];

    for (let i = 0; i < galaxies.length; i++) {
        for (let j = i + 1; j < galaxies.length; j++) {
            pairs.push([galaxies[i], galaxies[j]]);
        }
    }

    return pairs;
}

const memo = new Map<string, Map<string, number>>();

function pairDist(g: Graph<string>, a: string, b: string): number {
    if (!memo.has(a)) {
        const dists = dijkstra(g, a);
        memo.set(a, dists);
        return dists.get(b) ?? Infinity;
    }

    const dists = memo.get(a)!;
    return dists.get(b) ?? Infinity;
}

function galaxyPathLength(expansionFactor: number) {
    const universe = parseInput(input);
    const galaxies = findGalaxies(universe);
    const g = buildGraph(universe, expansionFactor);
    const pairs = getPairs(galaxies);
    memo.clear();

    let sum = 0;
    let index = 0;

    for (const [a, b] of pairs) {
        const dist = pairDist(g, a, b);
        sum += dist;
        index++;
    }

    return sum;
}

function part1() {
    return galaxyPathLength(2);
}

function part2() {
    return galaxyPathLength(1000000);
}

console.log(part1());
console.log(part2());
