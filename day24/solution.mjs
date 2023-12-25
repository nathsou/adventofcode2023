import { readFileSync } from 'fs';

const vec3 = (x, y, z = 0) => ({x, y, z});

function parseStones(stones) {
    return stones.split('\n').map(line => {
        const [pos, vel] = line.split('@').map(s => s.trim());
        return {
            pos: vec3(...pos.split(',').map(s => parseInt(s.trim()))),
            dir: vec3(...vel.split(',').map(s => parseInt(s.trim()))),
        };
    });
}

const stones = parseStones(readFileSync('./input.txt', 'utf-8'));

const toLineEquation = (pos, vel) => {
    const m = vel.y / vel.x;
    const b = pos.y - m * pos.x;
    return {m, b};
};

function lineIntersection(m1, b1, m2, b2) {
    const x = (b2 - b1) / (m1 - m2);
    const y = m1 * x + b1;

    return {x, y};
}

function parametricLineIntersection(pos1, vel1, pos2, vel2) {
    const t1 = (pos2.x - pos1.x) / (vel1.x - vel2.x);
    const t2 = (pos2.y - pos1.y) / (vel1.y - vel2.y);

    return { t1, t2 };
}

function* exclusivePairs(as, bs) {
    for (let i = 0; i < as.length; i++) {
        for (let j = i + 1; j < bs.length; j++) {
            yield [as[i], bs[j]];
        }
    }
}

function part1(min = 200000000000000, max = 400000000000000) {
    let count = 0;

    for (const [a, b] of exclusivePairs(stones, stones)) {
        const line1 = toLineEquation(a.pos, a.dir);
        const line2 = toLineEquation(b.pos, b.dir);
        const t = lineIntersection(line1.m, line1.b, line2.m, line2.b);
        const t1 = (t.x - a.pos.x) / a.dir.x;
        const t2 = (t.x - b.pos.x) / b.dir.x;

        if (t1 >= 0 && t2 >= 0 && t.x >= min && t.x <= max && t.y >= min && t.y <= max) {
            count += 1;
        }
    }

    return count;
}

// console.log(part1());

function lineBetween(p1, p2) {
    const dx = p2.x - p1.x;
    const dy = p2.y - p1.y;
    const dz = p2.z - p1.z;

    return {
        x0: p1.x,
        y0: p1.y,
        z0: p1.z,
        dx,
        dy,
        dz,
    };
}

function lineIntersection2(line1, line2) {
    const t1 = (line2.x0 - line1.x0) / line1.dx;
    const t2 = (line2.y0 - line1.y0) / line1.dy;
    const t3 = (line2.z0 - line1.z0) / line1.dz;

    const d1 = Math.abs(t1 - t2);
    const d2 = Math.abs(t2 - t3);

    if (d1 < 0.000001 && d2 < 0.000001) {
        return {t1, t2, t3};
    }

    return null;
}

function isOnLine(p, line, tolerance = 0.000001) {
    const t1 = (p.x - line.x0) / line.dx;
    const t2 = (p.y - line.y0) / line.dy;
    const t3 = (p.z - line.z0) / line.dz;

    const d1 = Math.abs(t1 - t2);
    const d2 = Math.abs(t2 - t3);

    return d1 < tolerance && d2 < tolerance;
}

function part2() {
    for (const [a, b] of exclusivePairs(stones, stones)) {
        const line = lineBetween(a.pos, b.pos);
        const line2 = { pos: a.pos, dir: a.dir };
        const count = stones.filter(s => {
            const inter = newtonsMethod(line2, { pos: s.pos, dir: s.dir }, 1, 1, 100 * Number.EPSILON, 10_000);
            return inter != null;
        }).length;
        console.log(line, count);
    }
}

function dotProduct(v1, v2) {
    return v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
}

function subtractVectors(v1, v2) {
    return { x: v1.x - v2.x, y: v1.y - v2.y, z: v1.z - v2.z };
}

function addVectors(v1, v2) {
    return { x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z };
}

function multiplyVector(v, scalar) {
    return { x: v.x * scalar, y: v.y * scalar, z: v.z * scalar };
}

function distanceSquared(r1, r2) {
    const diff = subtractVectors(r1, r2);
    return dotProduct(diff, diff);
}

function at(line, t) {
    return addVectors(line.pos, multiplyVector(line.dir, t));
}

function newtonsMethod(line1, line2, t1Start = 1, t2Start = 1, tolerance = 100 * Number.EPSILON, maxIterations = 1000) {
    let t1 = t1Start, t2 = t2Start;

    for (let i = 0; i < maxIterations; i++) {
        const r1 = addVectors(line1.pos, multiplyVector(line1.dir, t1));
        const r2 = addVectors(line2.pos, multiplyVector(line2.dir, t2));

        const f = distanceSquared(r1, r2);

        if (Math.abs(t2 - t1) < tolerance && Math.sqrt(f) < tolerance) {
            return { t: (t1 + t2) / 2, intersection: at(line1, t1) };
        }

        const dr1dt1 = line1.dir;
        const dr2dt2 = line2.dir;
        const dfdt1 = 2 * dotProduct(dr1dt1, subtractVectors(r1, r2));
        const dfdt2 = -2 * dotProduct(dr2dt2, subtractVectors(r1, r2));

        t1 -= f / dfdt1;
        t2 -= f / dfdt2;
    }

    return null;
}

// const line1 = {
//     pos: vec3(24, 13, 10),
//     dir: vec3(-3, 1, 2),
// };

// const line2 = {
//     pos: vec3(19, 13, 30),
//     dir: vec3(-2, 1, -2),
// };

// const inters = stones.map(s => {
//     const line2 = { pos: s.pos, dir: s.dir };
//     const intersection = newtonsMethod(line1, line2, 1, 1, 100 * Number.EPSILON, 10_000);
//     return intersection;
// });

// console.log(inters);
// console.log(newtonsMethod(line1, line2));

// const p1 = at({ point: p0, dir: d }, 2);
// console.log(p1);

function futurePositions(line, tMax) {
    const positions = [];

    for (let t = 0; t < tMax; t++) {
        positions.push(at(line, t));
    }

    return positions;
}

// console.log(futurePositions(stones[0], 10));
// console.log(futurePositions(stones[1], 10));

// Hailstone: 20, 19, 15 @ 1, -5, -3
// Collision time: 1
// Collision position: 21, 14, 12

// Hailstone: 18, 19, 22 @ -1, -1, -2
// Collision time: 3
// Collision position: 15, 16, 16

// Hailstone: 20, 25, 34 @ -2, -2, -4
// Collision time: 4
// Collision position: 12, 17, 18

// Hailstone: 19, 13, 30 @ -2, 1, -2
// Collision time: 5
// Collision position: 9, 18, 20

// Hailstone: 12, 31, 28 @ -1, -2, -1
// Collision time: 6
// Collision position: 6, 19, 22

function* pairs(as, bs) {
    for (let i = 0; i < as.length; i++) {
        for (let j = 0; j < bs.length; j++) {
            yield [as[i], bs[j]];
        }
    }
}

const ps = line => {
    const points = [];
    for (let i = 0; i < 100; i++) {
        points.push(vec3(line.pos.x + line.dir.x * i, line.pos.y + line.dir.y * i));
    }

    return points;
};

const pss = new Map([...stones.map(s => [s, ps(s)])]);

function yo(line1, line2) {
    const rs = new Set();
    const ps1 = pss.get(line1);
    const ps2 = pss.get(line2);
    for (const [p0, p1] of pairs(ps1, ps2)) {
        if (p0.x === p1.x && p0.y === p1.y) {
            continue;
        }

        const m = (p0.y - p1.y) / (p0.x - p1.x);
        const dx = 1 / m;
        const b = p0.y - m * p0.x;
        console.log(dx, m);

        if (Number.isInteger(m) && Number.isInteger(b) && Number.isFinite(m) && Number.isFinite(b)) {
            const key = `${dx}:${b}`;
            console.log(key);
            rs.add(key);
        }
    }

    return rs;
}

// function findCommonLine() {
//     let inter = null;

//     for (const [line1, line2] of exclusivePairs(stones, stones)) {
//         if (line1 !== line2) {
//             const rs = yo(line1, line2);

//             if (inter == null) {
//                 inter = rs;
//             } else {
//                 inter = new Set([...inter].filter(x => rs.has(x)));
//             }
//         }
//     }

//     return inter;
// }

// console.log(findCommonLine());

for (const [line1, line2] of exclusivePairs(stones, stones)) {
    if (line1 !== line2) {
        const rs = yo(line1, line2);
        // console.log(rs);
    }
}

// 18 - 1 * t2 = x0 + dx * t2