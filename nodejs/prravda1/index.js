const fs = require("fs");
const os = require("os");
const readline = require("readline");
const { OUTPUT_PATH, MEASUREMENTS_PATH, Timer } = require("../common");
const {
  isMainThread,
  Worker,
  workerData,
  parentPort,
} = require("worker_threads");

function minBigInt(a, b) {
  return a < b ? a : b;
}

function maxBigInt(a, b) {
  return a > b ? a : b;
}

/**
 * Basic idea: parrallelize the work by creating multiple worker threads
 * Each worker thread reads the measurements file and processes the lines
 * based on the index of the worker thread
 * The results from each worker thread are aggregated in the main thread
 * and then sorted by city name in ascending order
 * The aggregated results are then returned as a string
 *
 * But I can't solve the parraleization problem on hashmap
 * So I will solve it on array.
 *
 * prravda(https://github.com/prravda), 2024-04-21
 */

async function solution() {
  return new Promise((resolve) => {
    if (isMainThread) {
      // create a map to aggregate the results from all workers
      const cityMap = new Map();
      // get the number of worker threads to create based on the number of CPU cores
      const numOfWorkerThreads = Math.floor(os.cpus().length);

      // create a variable to keep track of the number of active workers
      let activeWorkers = numOfWorkerThreads;

      // create worker threads
      for (let i = 0; i < numOfWorkerThreads; i++) {
        const worker = new Worker(__filename, {
          workerData: { index: i, totalWorkers: numOfWorkerThreads },
        });

        // listen for messages from the worker threads
        worker.on("message", (data) => {
          data.forEach(({ cityName, min, max, total, count }) => {
            if (!cityMap.has(cityName)) {
              cityMap.set(cityName, { min, max, total: BigInt(total), count });
            } else {
              const existing = cityMap.get(cityName);

              existing.min = minBigInt(existing.min, min);
              existing.max = maxBigInt(existing.max, max);
              existing.total += BigInt(total);
              existing.count += count;
            }
          });
        });

        // listen for the exit event from the worker threads
        worker.on("exit", () => {
          activeWorkers--;
          // if all worker threads have exited, resolve the promise
          // and return the aggregated results into a string
          // after sorting the map by city name asc order
          if (activeWorkers === 0) {
            const result = Array.from(
              new Map([...cityMap.entries()].sort()),
              ([cityName, { min, max, total, count }]) =>
                `${cityName}=${min};${max};${
                  total / BigInt(count)
                }(${total}/${count})`
            ).join("\n");
            resolve(result + "\n");
          }
        });
      }
    } else {
      // worker thread logic
      const { index, totalWorkers } = workerData;

      // create a read stream to read the measurements file
      const stream = fs.createReadStream(MEASUREMENTS_PATH);
      const rl = readline.createInterface({
        input: stream,
        crlfDelay: Infinity,
      });

      // create an array to store the results from each worker thread
      const results = [];
      // create a counter to keep track of the line number
      let lineCounter = 0;

      // read each line from the file
      rl.on("line", (line) => {
        // distribute the lines to the worker threads based on the index
        if (lineCounter % totalWorkers === index) {
          const [cityName, measurementString] = line.split(";");
          const measurement = Number(measurementString);

          if (!isNaN(measurement)) {
            const existing = results.find(
              (result) => result.cityName === cityName
            );
            if (!existing) {
              results.push({
                cityName,
                min: measurement,
                max: measurement,
                total: measurement.toString(),
                count: 1,
              });
            } else {
              existing.min = minBigInt(existing.min, measurement);
              existing.max = maxBigInt(existing.max, measurement);
              existing.total = (
                BigInt(existing.total) + BigInt(measurement)
              ).toString();
              existing.count++;
            }
          }
        }
        lineCounter++;
      });

      // listen for the close event from the read stream
      rl.on("close", () => {
        // send the results back to the main thread
        parentPort.postMessage(results);
      });
    }
  });
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
