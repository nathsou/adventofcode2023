import { readFileSync } from "fs";

const input = readFileSync('input.txt').toString();

type Cell = '.' | 'O' | '#';

type Platform = Cell[][];

function parseInput(input: string): Platform {
    return input.split('\n').map(row => row.split('') as Cell[]);
}

function topMostFreeSpace(platform: Platform, x: number, y: number): number {
    let topMost = y;

    for (let j = y - 1; j >= 0; j--) {
        const cell = platform[j][x];
        if (cell === '.') {
            // console.log(`found free space at ${x},${j}`, platform[j][x]);
            topMost = j;
        } else if (cell === '#' || cell === 'O') {
            break;
        }
    }

    return topMost;
}

function leftMostFreeSpace(platform: Platform, x: number, y: number): number {
    let leftMost = x;

    for (let i = x - 1; i >= 0; i--) {
        const cell = platform[y][i];
        if (cell === '.') {
            // console.log(`found free space at ${i},${y}`, platform[y][i]);
            leftMost = i;
        } else if (cell === '#' || cell === 'O') {
            break;
        }
    }

    return leftMost;
}

function rightMostFreeSpace(platform: Platform, x: number, y: number): number {
    let rightMost = x;

    for (let i = x + 1; i < platform[y].length; i++) {
        const cell = platform[y][i];
        if (cell === '.') {
            // console.log(`found free space at ${i},${y}`, platform[y][i]);
            rightMost = i;
        } else if (cell === '#' || cell === 'O') {
            break;
        }
    }

    return rightMost;
}

function bottomMostFreeSpace(platform: Platform, x: number, y: number): number {
    let bottomMost = y;

    for (let j = y + 1; j < platform.length; j++) {
        const cell = platform[j][x];
        if (cell === '.') {
            // console.log(`found free space at ${x},${j}`, platform[j][x]);
            bottomMost = j;
        } else if (cell === '#' || cell === 'O') {
            break;
        }
    }

    return bottomMost;
}

function tiltNorth(platform: Platform): void {
    for (let j = 0; j < platform.length; j++) {
        for (let i = 0; i < platform[j].length; i++) {
            if (platform[j][i] === 'O') {
                const newRow = topMostFreeSpace(platform, i, j);
                // console.log(`tilting ${i},${j} to ${i},${newRow}`);
                if (newRow === j) continue;
                platform[newRow][i] = 'O';
                platform[j][i] = '.';
                // showPlatform(platform);
            }
        }
    }
}

function tiltSouth(platform: Platform): void {
    for (let j = platform.length - 1; j >= 0; j--) {
        for (let i = 0; i < platform[j].length; i++) {
            if (platform[j][i] === 'O') {
                const newRow = bottomMostFreeSpace(platform, i, j);
                if (newRow === j) continue;
                platform[newRow][i] = 'O';
                platform[j][i] = '.';
            }
        }
    }
}

function tiltWest(platform: Platform): void {
    for (let j = 0; j < platform.length; j++) {
        for (let i = 0; i < platform[j].length; i++) {
            if (platform[j][i] === 'O') {
                const newCol = leftMostFreeSpace(platform, i, j);
                if (newCol === i) continue;
                platform[j][newCol] = 'O';
                platform[j][i] = '.';
            }
        }
    }
}

function tiltEast(platform: Platform): void {
    for (let j = 0; j < platform.length; j++) {
        for (let i = platform[j].length - 1; i >= 0; i--) {
            if (platform[j][i] === 'O') {
                const newCol = rightMostFreeSpace(platform, i, j);
                if (newCol === i) continue;
                platform[j][newCol] = 'O';
                platform[j][i] = '.';
            }
        }
    }
}

function formatPlatform(platform: Platform): string {
    return platform.map(row => row.join('')).join('\n');
}

function showPlatform(platform: Platform): void {
    console.log(formatPlatform(platform));
    console.log();
}

function getTotalLoad(platform: Platform): number {
    let totalLoad = 0;

    for (let j = 0; j < platform.length; j++) {
        const rowLoad = platform.length - j;
        for (let i = 0; i < platform[j].length; i++) {
            if (platform[j][i] === 'O') {
                totalLoad += rowLoad;
            }
        }
    }

    return totalLoad;
}

function part1() {
    const platform = parseInput(input);
    tiltNorth(platform);
    return getTotalLoad(platform);
}

function titleCycle(platform: Platform) {
    tiltNorth(platform);
    tiltWest(platform);
    tiltSouth(platform);
    tiltEast(platform);
}

// part1();

function part2() {
    const platform = parseInput(input);
    const M = 1000000000;
    const seen = new Map<string, number[]>();
    let i = 0;

    while (true) {
        const hash = formatPlatform(platform);

        if (seen.has(hash)) {
            const indices = seen.get(hash)!;
            indices.push(i);
            if (indices.length > 2) {
                break;
            }
        } else {
            seen.set(hash, [i]);
        }

        titleCycle(platform);
        i += 1;
    }

    // remove the patterns only seen once
    for (const [key, value] of seen.entries()) {
        if (value.length === 1) {
            seen.delete(key);
        }
    }

    const [cycleStart, next] = [...seen.values()][0];
    const period = next - cycleStart;
    const n = (M - cycleStart) % period;
    const finalPlatform = [...seen.entries()].find(([_, value]) => value.includes(n + cycleStart))![0];

    return getTotalLoad(parseInput(finalPlatform));
}

console.log(part1());
console.log(part2());