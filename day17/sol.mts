import { Graph } from "./graphs";
import { Heap } from "./heaps";

const inputText = await Bun.file('input.txt').text();
const blocks = inputText.split('\n').map(line => line.split('').map(Number));

type Dir = '<' | '>' | '^' | 'v';
const DIRS: Dir[] = ['<', '>', '^', 'v'];

const h = (x: number, y: number, dir: Dir, rem: number) => `${x}:${y}:${dir}:${rem}`;

const buildGraph1 = (blocks: number[][]) => {
    const g = new Graph<string>();
    const width = blocks[0].length;
    const height = blocks.length;

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            for (const dir of DIRS) {
                for (let rem = 3; rem >= 0; rem--) {
                    const label = h(x, y, dir, rem);
                    g.insertVertex(label);
                    for (const d of allowedDirs[dir]) {
                        const [dx, dy] = DIR_MAPPING[d];
                        const xx = x + dx;
                        const yy = y + dy;
                        if (!(xx < 0 || xx >= width || yy < 0 || yy >= height)) {
                            if (d === dir) {
                                for (let r = rem - 1; r > 0; r--) {
                                    g.insertDirectedEdge(label, h(xx, yy, d, r), blocks[yy][xx]);
                                }
                            } else {
                                g.insertDirectedEdge(label, h(xx, yy, d, 3), blocks[yy][xx]);
                            }
                        }
                    }
                }
            }
        }
    }

    return g;
};

const DIR_MAPPING: Record<Dir, [number, number]> = {
    '<': [-1, 0],
    '>': [1, 0],
    '^': [0, -1],
    'v': [0, 1],
};

const allowedDirs: Record<Dir, Dir[]> = {
    '<': ['<', '^', 'v'],
    '>': ['>', '^', 'v'],
    '^': ['^', '<', '>'],
    'v': ['v', '<', '>'],
};

function dijkstra2<Label = string>(g: Graph<Label>, sources: Set<Label>) {
    const unvisited = new Heap<Label, number>(
        g.getVertices().map(v => [v, sources.has(v) ? 0 : Infinity]),
        (a, b) => a < b
    );

    const visited = new Set<Label>();
    const distances = new Map<Label, number>(unvisited.getData());
    const parents = new Map<Label, Label>();

    while (!unvisited.empty()) {
        const [u, d] = unvisited.removeHighestPriority();
        if (d === Infinity) break;

        for (const v of g.adjacentVertices(u)) {
            if (!visited.has(v)) {
                const newDist = d + g.getCost(u, v);
                if (newDist < distances.get(v)!) {
                    distances.set(v, newDist);
                    unvisited.updatePriority(v, newDist);
                    parents.set(v, u);
                }
            }
        }

        visited.add(u);
    }

    return { distances, parents };
}

const buildGraph2 = (blocks: number[][]) => {
    const g = new Graph<string>();
    const width = blocks[0].length;
    const height = blocks.length;
    const MIN = 4;
    const MAX = 10;

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            for (const dir of DIRS) {
                for (let rem = MAX; rem >= 0; rem--) {
                    const label = h(x, y, dir, rem);
                    g.insertVertex(label);
                    for (const d of allowedDirs[dir]) {
                        const canTurn = rem <= (MAX - MIN);
                        const [dx, dy] = DIR_MAPPING[d];
                        const xx = x + dx;
                        const yy = y + dy;

                        if (!(xx < 0 || xx >= width || yy < 0 || yy >= height)) {
                            if (d === dir) {
                                g.insertDirectedEdge(label, h(xx, yy, d, rem - 1), blocks[yy][xx]);
                                // for (let r = rem - 1; r >= 0; r--) {
                                //     g.insertDirectedEdge(label, h(xx, yy, d, r), blocks[yy][xx]);
                                // }
                            } else if (canTurn) {
                                g.insertDirectedEdge(label, h(xx, yy, d, MAX - 1), blocks[yy][xx]);
                            }
                        }
                    }
                }
            }
        }
    }

    return g;
};

function part1() {
    const width = blocks[0].length;
    const height = blocks.length;
    const g = buildGraph1(blocks);
    const sources = new Set<string>([
        h(0, 0, '>', 3),
        h(0, 0, 'v', 3),
    ]);

    const dists = dijkstra2(g, sources).distances;
    return Math.min(
        ...[...dists.entries()]
        .filter(([k, d]) => d !== Infinity && k.startsWith(`${width - 1}:${height - 1}`))
        .map(([_, d]) => d)
    );
}

function printPath(
    width: number,
    height: number,
    parents: Map<string, string>,
    target: string
) {
    const lines: string[][] = [];

    for (let y = 0; y < height; y++) {
        lines.push([]);
        for (let x = 0; x < width; x++) {
            lines[y].push('.');
        }
    }

    const path = [];
    let cur = target;
    while (cur) {
        path.push(cur);
        cur = parents.get(cur)!;
    }

    for (const p of path.toReversed()) {
        const [x, y, dir] = p.split(':');
        lines[Number(y)][Number(x)] = dir;
    }

    for (const line of lines) {
        console.log(line.join(''));
    }
}


function part2() {
    const width = blocks[0].length;
    const height = blocks.length;
    const g = buildGraph2(blocks);
    const sources = new Set<string>([
        h(0, 0, '>', 10),
        h(0, 0, 'v', 10),
    ]);

    const {distances, parents} = dijkstra2(g, sources);
    const dst = [...distances.entries()]
    .filter(([k, d]) => d !== Infinity && k.startsWith(`${width - 1}:${height - 1}`));
    
    const minDist = Math.min(...dst.map(([_, d]) => d));
    const minLabel = dst.find(([_, d]) => d === minDist)![0];
    // printPath(width, height, parents, minLabel);

    return minDist;
}

console.log(part1());
console.log(part2());
