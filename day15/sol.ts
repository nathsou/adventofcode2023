import { readFileSync } from "fs";

const input = readFileSync('input.txt').toString();

type Input = string[];
type Instr = { type: '=', label: string, focalLength: number } | { type: '-', label: string };

function parseInstr(instr: string): Instr {
    if (instr.endsWith('-')) {
        return { type: '-', label: instr.slice(0, -1) };
    }

    const [label, focalLength] = instr.split('=');

    return { type: '=', label, focalLength: Number(focalLength) };
}

function parseInput1(input: string): Input {
    return input.replaceAll('\n', '').split(',');
}

function parseInput2(input: string): Instr[] {
    return input.replaceAll('\n', '').split(',').map(parseInstr);
}

function hash(str: string): number {
    let curr = 0;

    for (let i = 0; i < str.length; i++) {
        const ascii = str.charCodeAt(i);
        curr += ascii;
        curr *= 17;
        curr %= 256;
    }

    return curr;
}

class HashMap {
    boxes: { label:string, value: number }[][];
    constructor() {
        this.boxes = [];

        for (let i = 0; i < 256; i++) {
            this.boxes.push([]);
        }
    }

    insert(label: string, value: number) {
        const h = hash(label);
        const box = this.boxes[h];
        const idx = box.findIndex(item => item.label === label);

        if (idx !== -1) {
            box[idx].value = value;
            return;
        } else {
            this.boxes[h].push({ label, value });
        }
    }

    remove(label: string) {
        const h = hash(label);
        const box = this.boxes[h];
        const idx = box.findIndex(item => item.label === label);

        if (idx !== -1) {
            box.splice(idx, 1);
        }
    }
}

function sum(ns: number[]): number {
    return ns.reduce((acc, n) => acc + n, 0);
}

function part1() {
    return sum(parseInput1(input).map(hash));
}

function part2() {
    const instrs = parseInput2(input);
    const hm = new HashMap();

    for (const inst of instrs) {
        if (inst.type === '-') {
            hm.remove(inst.label);
        } else {
            hm.insert(inst.label, inst.focalLength);
        }
    }

    let focusinPower = 0;

    for (let i = 0; i < hm.boxes.length; i++) {
        focusinPower += (1 + i) * sum(hm.boxes[i].map((item, idx) => (idx + 1) * item.value));
    }

    return focusinPower;
}

console.log(part1());
console.log(part2());
