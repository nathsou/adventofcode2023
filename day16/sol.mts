
type Tile = '.' | '/' | '\\' | '|' | '-';
type Beam = { x: number, y: number, dx: number, dy: number };
type Dir = '<' | '>' | '^' | 'v' | '.';
const inputText = await Bun.file('input.txt').text();
const h = (x: number, y: number) => `${x}:${y}`;

function parseTiles(input: string): Tile[][] {
    return input.split('\n').map(line => line.split('') as Tile[]);
}

function getDir(dx: number, dy: number): Dir {
    if (dx === 1 && dy === 0) return '>';
    if (dx === -1 && dy === 0) return '<';
    if (dx === 0 && dy === 1) return 'v';
    if (dx === 0 && dy === -1) return '^';
    return '.';
}

function pushMap<K, V>(key: K, value: V, map: Map<K, V[]>) {
    if (!map.has(key)) {
        map.set(key, []);
    }

    map.get(key)!.push(value);
}

function swapRemove(elems: unknown[], index: number): void {
    if (elems.length <= 1) { elems.pop(); } else { elems[index] = elems.pop()!; }
}

function countEnergizedTiles(tiles: Tile[][], startBeam: Beam) {
    const energized = new Map<string, Dir[]>();
    const beams: Beam[] = [startBeam];
    const width = tiles[0].length;
    const height = tiles.length;

    while (beams.length > 0) {
        for (let i = 0; i < beams.length; i++) {
            const b = beams[i];
            const key = h(b.x, b.y);
            const dir = getDir(b.dx, b.dy);
            const outOfBounds = b.x < 0 || b.x >= width || b.y < 0 || b.y >= height;
            const remove = (index: number) => swapRemove(beams, index);

            if (outOfBounds || energized.get(key)?.includes(dir)) {
                remove(i);
            } else {
                pushMap(key, dir, energized);

                switch (tiles[b.y][b.x]) {
                    case '.':
                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    case '/': {
                        const prevDx = b.dx;

                        b.dx = -b.dy;
                        b.dy = -prevDx;

                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    }
                    case '\\': {
                        const prevDx = b.dx;

                        b.dx = b.dy;
                        b.dy = prevDx;

                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    }
                    case '|':
                        if (b.dx !== 0) {
                            remove(i);
                            beams.push(
                                {
                                    x: b.x,
                                    y: b.y - 1,
                                    dx: 0,
                                    dy: -1,
                                },
                                {
                                    x: b.x,
                                    y: b.y + 1,
                                    dx: 0,
                                    dy: 1,
                                }
                            );
                        } else {
                            b.x += b.dx;
                            b.y += b.dy;
                        }
                        break;
                    case '-':
                        if (b.dy !== 0) {
                            remove(i);
                            beams.push(
                                {
                                    x: b.x - 1,
                                    y: b.y,
                                    dx: -1,
                                    dy: 0,
                                },
                                {
                                    x: b.x + 1,
                                    y: b.y,
                                    dx: 1,
                                    dy: 0,
                                }
                            );
                        } else {
                            b.x += b.dx;
                            b.y += b.dy;
                        }
                        break;
                }
            }
        }
    }

    return energized.size;
}

function part1() {
    const tiles = parseTiles(inputText);
    return countEnergizedTiles(tiles, { x: 0, y: 0, dx: 1, dy: 0 });
}

function part2() {
    const tiles = parseTiles(inputText);
    const width = tiles[0].length;
    const height = tiles.length;
    let maxEnergy = 0;

    function traverseEdge(startX: number, startY: number, dx: number, dy: number, dirX: number, dirY: number): void {
        for (let x = startX, y = startY; x >= 0 && x < width && y >= 0 && y < height; x += dx, y += dy) {
            maxEnergy = Math.max(maxEnergy, countEnergizedTiles(tiles, { x, y, dx: dirX, dy: dirY }));
        }
    }

    traverseEdge(0, 0, 1, 0, 0, 1); // top edge
    traverseEdge(0, 0, 0, 1, 1, 0); // left edge
    traverseEdge(width - 1, height - 1, -1, 0, 0, -1); // bottom edge
    traverseEdge(width - 1, height - 1, 0, -1, -1, 0); // right edge

    return maxEnergy;
}

console.log(part1());
console.log(part2());
