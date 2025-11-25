const { parentPort, workerData } = require("worker_threads");
const fs = require("fs");

class Stats {
    constructor(measurement) {
        this.min = measurement;
        this.max = measurement;
        this.total = measurement;
        this.count = 1;
    }
    add(measurement) {
        if (measurement < this.min) this.min = measurement;
        if (measurement > this.max) this.max = measurement;
        this.total += measurement;
        this.count += 1;
    }
}

function parseIntInline(data, start, end) {
    let n = 0;
    for (let i = start; i < end; i++) {
        const code = data.charCodeAt(i) - 48;
        n = n * 10 + code;
    }
    return n;
}


async function run() {
    const { filePath, start, end, highWaterMark } = workerData;

    const map = new Map();

    await new Promise((resolve, reject) => {
        const stream = fs.createReadStream(filePath, {
            start,
            end,
            highWaterMark,
            encoding: "utf8",
        });

        let leftover = "";

        stream.on("data", (chunk) => {
            let data = leftover + chunk;
            let cursor = 0;
            while (true) {
                const nl = data.indexOf("\n", cursor);
                if (nl === -1) break;

                if (nl === cursor) {
                    cursor = nl + 1;
                    continue;
                }

                const sep = data.indexOf(";", cursor);
                if (sep === -1 || sep > nl) {
                    cursor = nl + 1;
                    continue;
                }

                const city = data.slice(cursor, sep);
                const m = parseIntInline(data, sep + 1, nl);

                let s = map.get(city);
                if (s) {
                    s.add(m);
                } else {
                    map.set(city, new Stats(m));
                }

                cursor = nl + 1;
            }

            leftover = data.slice(cursor);
        });

        stream.on("end", () => {
            if (leftover.length > 0) {
                const data = leftover;
                let cursor = 0;
                const len = data.length;

                if (len > 0) {
                    const nl = data.indexOf("\n", cursor);
                    const lineEnd = nl === -1 ? len : nl;

                    if (lineEnd > cursor) {
                        const sep = data.indexOf(";", cursor);
                        if (sep !== -1 && sep < lineEnd) {
                            const city = data.slice(cursor, sep);
                            const m = parseIntInline(data, sep + 1, lineEnd);

                            let s = map.get(city);
                            if (s) s.add(m);
                            else map.set(city, new Stats(m));
                        }
                    }
                }
            }
            resolve();
        });

        stream.on("error", reject);
    });

    const result = {};
    for (const [city, s] of map.entries()) {
        result[city] = {
            min: s.min,
            max: s.max,
            total: s.total,
            count: s.count,
        };
    }

    parentPort.postMessage(result);
}

run().catch((err) => {
    console.error("Worker error:", err);
    throw err;
});
