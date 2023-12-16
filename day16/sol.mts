

type Tile = '.' | '/' | '\\' | '|' | '-';
const inputText = await Bun.file('input.txt').text();
const log = console.log;

function parseTiles(input: string): Tile[][] {
    return input.split('\n').map(line => line.split('') as Tile[]);
}

type Beam = {
    x: number,
    y: number,
    dx: number,
    dy: number,
};

const h = (x: number, y: number) => `${x}:${y}`;

type Dir = '<' |  '>' | '^' | 'v' | '.';

function getDir(dx: number, dy: number): Dir {
    if (dx === 1 && dy === 0) {
        return '>';
    }

    if (dx === -1 && dy === 0) {
        return '<';
    }

    if (dx === 0 && dy === 1) {
        return 'v';
    }

    if (dx === 0 && dy === -1) {
        return '^';
    }

    return '.';
}

function pushMap<K, V>(key: K, value: V, map: Map<K, V[]>) {
    if (!map.has(key)) {
        map.set(key, []);
    }

    map.get(key)!.push(value);
}

function countEnergizedTiles(tiles: Tile[][], startBeam: Beam) {
    const energized = new Map<string, Dir[]>();
    const beams: Beam[] = [startBeam];

    const width = tiles[0].length;
    const height = tiles.length;

    while (beams.length > 0) {
        for (let i = beams.length - 1; i >= 0; i--) {
            const b = beams[i];
            const key = h(b.x, b.y);
            const dir = getDir(b.dx, b.dy);
            if (energized.get(key)?.includes(dir)) {
                beams.splice(i, 1);
                continue;
            }

            if (b.x >= 0 && b.x < width && b.y >= 0 && b.y < height) {
                pushMap(key, dir, energized);
                const tile = tiles[b.y][b.x];

                switch (tile) {
                    case '.':
                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    case '/': {
                        const prevDx = b.dx;
                        const prevDy = b.dy;

                        b.dx = -prevDy;
                        b.dy = -prevDx;

                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    }
                    case '\\': {
                        const prevDx = b.dx;
                        const prevDy = b.dy;
                        b.dx = prevDy;
                        b.dy = prevDx;

                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    }
                    case '|':
                        if (b.dx !== 0) {
                            beams.splice(i, 1);

                            beams.push({
                                x: b.x,
                                y: b.y - 1,
                                dx: 0,
                                dy: -1,
                            });

                            beams.push({
                                x: b.x,
                                y: b.y + 1,
                                dx: 0,
                                dy: 1,
                            });
                        } else {
                            b.x += b.dx;
                            b.y += b.dy;
                        }
                        break;
                    case '-':
                        if (b.dy !== 0) {
                            beams.splice(i, 1);

                            beams.push({
                                x: b.x - 1,
                                y: b.y,
                                dx: -1,
                                dy: 0,
                            });

                            beams.push({
                                x: b.x + 1,
                                y: b.y,
                                dx: 1,
                                dy: 0,
                            });
                        } else {
                            b.x += b.dx;
                            b.y += b.dy;
                        }
                        break;
                }
            } else {
                beams.splice(i, 1);
            }
        }
    }

    return energized.size;
}

function part1() {
    const tiles = parseTiles(inputText);
    return countEnergizedTiles(tiles, {
        x: 0,
        y: 0,
        dx: 1,
        dy: 0,
    });
}

function part2() {
    const tiles = parseTiles(inputText);
    const width = tiles[0].length;
    const height = tiles.length;

    let maxEnergy = 0;

    // left edge
    for (let y = 0; y < height; y++) {
        const energy = countEnergizedTiles(tiles, {
            x: 0,
            y,
            dx: 1,
            dy: 0,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
        }
    }

    // right edge
    for (let y = 0; y < height; y++) {
        const energy = countEnergizedTiles(tiles, {
            x: width - 1,
            y,
            dx: -1,
            dy: 0,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
        }
    }


    // top edge
    for (let x = 0; x < width; x++) {
        const energy = countEnergizedTiles(tiles, {
            x,
            y: 0,
            dx: 0,
            dy: 1,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
        }
    }

    // bottom edge
    for (let x = 0; x < width; x++) {
        const energy = countEnergizedTiles(tiles, {
            x,
            y: height - 1,
            dx: 0,
            dy: -1,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
        }
    }

    return maxEnergy;
}

log(part1());
log(part2());
