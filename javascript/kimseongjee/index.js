const fs = require("fs");
const fsp = fs.promises;
const os = require("os");
const path = require("path");
const { Worker } = require("worker_threads");

const { MEASUREMENTS_PATH, OUTPUT_PATH, Timer } = require("../common");

function mergeStats(target, partial) {
    for (const [city, s] of Object.entries(partial)) {
        const min = s.min;
        const max = s.max;
        const total = s.total;
        const count = s.count;

        if (!target[city]) {
            target[city] = { min, max, total, count };
        } else {
            const t = target[city];
            if (min < t.min) t.min = min;
            if (max > t.max) t.max = max;
            t.total += total;
            t.count += count;
        }
    }
}

function runWorker(workerPath, filePath, start, end, highWaterMark) {
    return new Promise((resolve, reject) => {
        const worker = new Worker(workerPath, {
            workerData: {
                filePath,
                start,
                end,
                highWaterMark,
            },
        });

        worker.on("message", (msg) => resolve(msg));
        worker.on("error", reject);
        worker.on("exit", (code) => {
            if (code !== 0) {
                reject(new Error(`Worker exited with code ${code}`));
            }
        });
    });
}

async function buildOffsets(filePath, desiredWorkers) {
    const stat = await fsp.stat(filePath);
    const size = stat.size;

    if (desiredWorkers <= 1 || size === 0) {
        return [size];
    }

    const chunkSize = Math.floor(size / desiredWorkers);
    const BUFFER_SIZE = 128;
    const fd = await fsp.open(filePath, "r");
    const buffer = Buffer.alloc(BUFFER_SIZE);

    const offsets = [];
    let offset = 0;

    while (true) {
        offset += chunkSize;

        if (offset >= size || offset < 0) {
            offsets.push(size);
            break;
        }

        const { bytesRead } = await fd.read(buffer, 0, BUFFER_SIZE, offset);
        if (bytesRead === 0) {
            offsets.push(size);
            break;
        }

        const pos = buffer.indexOf(10);
        buffer.fill(0);

        if (pos !== -1) {
            offset += pos + 1;
            offsets.push(offset);
        } else {
            offsets.push(size);
            break;
        }
    }

    await fd.close();
    return offsets;
}

async function solution(workerCountRequested) {
    const offsets = await buildOffsets(MEASUREMENTS_PATH, workerCountRequested);
    const actualWorkers = offsets.length;

    const workerPath = path.join(__dirname, "worker.js");
    const workerPromises = [];

    const totalMem = os.totalmem();

    const perWorkerBudget = totalMem * 0.5 / actualWorkers;
    const defaultHWM = 4 * 1024 * 1024;
    const highWaterMark =
        Math.max(256 * 1024, Math.min(perWorkerBudget / 4, defaultHWM));
    for (let i = 0; i < actualWorkers; i++) {
        const start = i === 0 ? 0 : offsets[i - 1];
        const endInclusive = offsets[i] - 1;

        workerPromises.push(
            runWorker(
                workerPath,
                MEASUREMENTS_PATH,
                start,
                endInclusive,
                highWaterMark
            )
        );
    }

    const partials = await Promise.all(workerPromises);

    const merged = {};
    for (const part of partials) {
        mergeStats(merged, part);
    }

    const cities = Object.keys(merged).sort();
    let out = "";
    for (const city of cities) {
        const s = merged[city];
        const avg = Math.floor(s.total / s.count);
        out += `${city}=${s.min};${s.max};${avg}(${s.total}/${s.count})\n`;
    }

    return out;
}

async function main() {
    const expectOutput = String(fs.readFileSync(OUTPUT_PATH));

    const workerCount = os.cpus().length;


    const timer = new Timer();
    const got = await solution(workerCount);

    console.log(`Elapsed: ${timer.elapsedAsMilliSeconds()}ms`);

    if (got === expectOutput) {
        console.log("Test passed");
    } else {
        console.log("Test failed");
        console.log("Expected:");
        console.log(expectOutput);
        console.log("Actual:");
        console.log(got);
    }
}

main();
