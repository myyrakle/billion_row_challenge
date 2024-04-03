const fs = require('fs');
const readline = require('readline');
const { OUTPUT_PATH, MEASUREMENTS_PATH, Timer } = require('./common');

async function solution() {
    const instream = fs.createReadStream(MEASUREMENTS_PATH);

    const rl = readline.createInterface({
        input: instream,
        crlfDelay: Infinity
    });

    const map = {}
    for await (const line of rl) {
        let [cityName, measurement] = line.split(';');
        measurement = parseFloat(measurement);

        if (map[cityName]) {
            map[cityName].min = Math.min(map[cityName].min, measurement);
            map[cityName].max = Math.max(map[cityName].max, measurement);
            map[cityName].total += measurement;
            map[cityName].count++;
        } else {
            map[cityName] = {
                min: measurement,
                max: measurement,
                total: measurement,
                count: 1
            };
        }
    }


    let bucket = "";
    const list = Object.entries(map)
    list.sort((l, r) => l[0].localeCompare(r[0], undefined, { sensitivity: 'base' }));
    for (const [city_name, status] of list) {
        const avg = status.total / status.count;
        const line = `${city_name}=${status.min.toFixed(1)};${status.max.toFixed(1)};${avg.toFixed(1)}(${status.total.toFixed(1)}/${status.count})\n`;

        bucket += line;
    }

    return bucket;
}

async function main() {
    const expectOutput = String(fs.readFileSync(OUTPUT_PATH));

    const timer = new Timer();

    const got = await solution();

    console.log(`Elapsed: ${timer.elapsedAsMilliSeconds()}ms`);

    if (expectOutput === got) {
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