import { readFileSync } from "fs";
import { Graph, dijkstra } from './graphs';

const input = readFileSync("input.txt", "utf8");
const START_PIPE = 'L';  // depends on the input

function parseMaze(input: string): { maze: Maze, startX: number, startY: number } {
    const maze = input.split("\n").map((line) => line.split(''));
    const [startX, startY] = startPosition(maze);
    maze[startY][startX] = START_PIPE;
    return { maze, startX, startY };
}

type Maze = string[][];
type Vec2 = [number, number];
type Dir = 'N' | 'S' | 'E' | 'W';

function startPosition(maze: string[][]): Vec2 {
    for (let y = 0; y < maze.length; y++) {
        const x = maze[y].indexOf("S");
        if (x !== -1) {
            return [x, y];
        }
    }

    throw new Error("no start");
}

function label([x, y]: Vec2): string {
    return `${x}:${y}`;
}

function connections(pipe: string): Dir[] {
    switch (pipe) {
        case "|": return ['N', 'S'];
        case "-": return ['E', 'W'];
        case "L": return ['N', 'E'];
        case "J": return ['N', 'W'];
        case "7": return ['S', 'W'];
        case "F": return ['S', 'E'];
        default: return [];
    }
}

function at(maze: string[][], x: number, y: number): string {
    if (x < 0 || y < 0 || y >= maze.length || x >= maze[y].length) {
        return ".";
    }

    return maze[y][x];
}

const DELTAS: Record<Dir, Vec2> = {
    'N': [0, -1],
    'S': [0, 1],
    'E': [1, 0],
    'W': [-1, 0],
};

// construct maze graph
function buildGraph(maze: string[][]): Graph<string> {
    const graph = new Graph<string>();

    for (let y = 0; y < maze.length; y++) {
        for (let x = 0; x < maze[y].length; x++) {
            graph.insertVertex(label([x, y]));
            const pipe = maze[y][x];

            for (const dir of connections(pipe)) {
                const [dx, dy] = DELTAS[dir];
                const [nx, ny] = [x + dx, y + dy];
                const neighbor = at(maze, nx, ny);
                if (neighbor !== '.' && canConnect(pipe, neighbor, dir)) {
                    graph.insertUndirectedEdge(`${x}:${y}`, `${nx}:${ny}`, 1);
                }
            }
        }
    }

    return graph;
}

const CONNECTIONS: Record<string, Record<Dir, string[]>> = {
    '|': {
        'N': ['|', 'F', '7'],
        'S': ['|', 'L', 'J'],
        'E': [],
        'W': [],
    },
    '-': {
        'N': [],
        'S': [],
        'E': ['-', 'J', '7'],
        'W': ['-', 'F', 'L'],
    },
    'L': {
        'N': ['|', 'F', '7'],
        'S': [],
        'E': ['-', 'J', '7'],
        'W': [],
    },
    'J': {
        'N': ['|', 'F', '7'],
        'S': [],
        'E': [],
        'W': ['-', 'F', 'L'],
    },
    '7': {
        'N': [],
        'S': ['|', 'L', 'J'],
        'E': [],
        'W': ['-', 'F', 'L'],
    },
    'F': {
        'N': [],
        'S': ['|', 'L', 'J'],
        'E': ['-', 'J', '7'],
        'W': [],
    },
};

function canConnect(a: string, b: string, dir: Dir): boolean {
    return CONNECTIONS[a][dir].includes(b);
}

function mainLoop(g: Graph<string>, start: string) {
    const queue = [start];
    const visited = new Set<string>();

    while (queue.length > 0) {
        const u = queue.shift()!;
        if (!visited.has(u)) {
            visited.add(u);
            for (const v of g.adjacentVertices(u)) {
                queue.push(v);
            }
        }
    }

    return visited;
}

// a tile is enclosed if the number of wall in any direction is odd
// LJ and F7 form two walls
// L7 and FJ form one wall
function isEnclosed(maze: Maze, x0: number, y0: number, loop: Set<string>): boolean {
    return maze[y0]
        .map((p, x) => loop.has(`${x}:${y0}`) ? p : '.')
        .slice(x0)
        .join('')
        .replaceAll('.', '')
        .replaceAll('-', '')
        .replaceAll('LJ', '||')
        .replaceAll('F7', '||')
        .replaceAll('L7', '|')
        .replaceAll('FJ', '|')
        .length % 2 === 1;
}

function part1() {
    const { maze, startX, startY } = parseMaze(input);
    const g = buildGraph(maze);
    const loop = mainLoop(g, label([startX, startY]));
    const dists = dijkstra(g, label([startX, startY]));
    return Math.max(
        ...[...dists.entries()]
            .filter(([label, d]) => d !== Infinity && loop.has(label))
            .map(([_, d]) => d)
    );
}

function part2() {
    const { maze, startX, startY } = parseMaze(input);
    const g = buildGraph(maze);
    const loop = mainLoop(g, label([startX, startY]));
    let enclosedCount = 0;

    for (let y = 0; y < maze.length; y++) {
        for (let x = 0; x < maze[y].length; x++) {
            const label = `${x}:${y}`;
            if (!loop.has(label) && isEnclosed(maze, x, y, loop)) {
                enclosedCount += 1;
            }
        }
    }

    return enclosedCount;
}

console.log(part1());
console.log(part2());
