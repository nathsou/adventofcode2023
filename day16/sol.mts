

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

function countEnergizedTiles(tiles: Tile[][], startBeam: Beam) {
    const energized = new Map<string, number>();
    const beams: Beam[] = [startBeam];

    const width = tiles[0].length;
    const height = tiles.length;
    // let steps = 100;

    while (beams.length > 0) {
        for (let i = beams.length - 1; i >= 0; i--) {
            const b = beams[i];
            if ((energized.get(h(b.x, b.y)) ?? 0) > 100 ) {
                // console.log('already energized', b.x, b.y, b.dx, b.dy);
                beams.splice(i, 1);
            }

            if (b.x >= 0 && b.x < width && b.y >= 0 && b.y < height) {
                energized.set(h(b.x, b.y), (energized.get(h(b.x, b.y)) ?? 0) + 1);
                const tile = tiles[b.y][b.x];

                switch (tile) {
                    case '.':
                        b.x += b.dx;
                        b.y += b.dy;
                        break;
                    case '/': {
                        // log('splitting /', b.x, b.y, b.dx, b.dy);
                        const prevDx = b.dx;
                        const prevDy = b.dy;

                        b.dx = -prevDy;
                        b.dy = -prevDx;

                        b.x += b.dx;
                        b.y += b.dy;
                        // log('result', b.x, b.y, b.dx, b.dy);
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
                            // log('splitting |', b.x, b.y, b.dx, b.dy);
                            // remove this beam
                            beams.splice(i, 1);

                            // add new beams
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
                            // log('splitting -', b.x, b.y, b.dx, b.dy);
                            // remove this beam
                            beams.splice(i, 1);

                            // add new beams
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
                // log('out of bounds', b.x, b.y, b.dx, b.dy);
                beams.splice(i, 1);
            }
        }
    }

    // log(drawTiles(tiles, energized));
    // log(energized.size);

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

function drawTiles(tiles: Tile[][], energized: Map<string, number>) {
    const tilesCopy: (Tile | '#')[][] = tiles.map(line => [...line]);

    for (const tile of energized.keys()) {
        const [x, y] = tile.split(':').map(Number);
        if (tilesCopy[y][x] === '.') {
            tilesCopy[y][x] = '#';
        }
    }

    return tilesCopy.map(line => line.join('')).join('\n');
}

function part2() {
    const tiles = parseTiles(inputText);
    const width = tiles[0].length;
    const height = tiles.length;

    let maxEnergy = 0;
    let maxIndex = -1;

    // left edge
    for (let y = 0; y < height; y++) {
        log(`left edge ${y}/${height}`);
        const energy = countEnergizedTiles(tiles, {
            x: 0,
            y,
            dx: 1,
            dy: 0,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
            maxIndex = y;
        }
    }

    // right edge
    for (let y = 0; y < height; y++) {
        log(`right edge ${y}/${height}`);
        const energy = countEnergizedTiles(tiles, {
            x: width - 1,
            y,
            dx: -1,
            dy: 0,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
            maxIndex = y;
        }
    }


    // top edge
    for (let x = 0; x < width; x++) {
        log(`top edge ${x}/${width}`);
        const energy = countEnergizedTiles(tiles, {
            x,
            y: 0,
            dx: 0,
            dy: 1,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
            maxIndex = x;
        }
    }

    // bottom edge
    for (let x = 0; x < width; x++) {
        log(`bottom edge ${x}/${width}`);
        const energy = countEnergizedTiles(tiles, {
            x,
            y: height - 1,
            dx: 0,
            dy: -1,
        });

        if (energy > maxEnergy) {
            maxEnergy = energy;
            maxIndex = x;
        }
    }

    return maxEnergy;
}

log(part1());
log(part2());
