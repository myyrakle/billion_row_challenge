const fs = require("fs");
const readline = require("readline");
const { OUTPUT_PATH, MEASUREMENTS_PATH, Timer } = require("./common");

function minBigInt(a, b) {
  return a < b ? a : b;
}

function maxBigInt(a, b) {
  return a > b ? a : b;
}

async function solution() {
  const instream = fs.createReadStream(MEASUREMENTS_PATH);

  const rl = readline.createInterface({
    input: instream,
    crlfDelay: Infinity,
  });

  const map = {};
  for await (const line of rl) {
    let [cityName, measurement] = line.split(";");
    measurement = BigInt(measurement);

    if (map[cityName]) {
      map[cityName].min = minBigInt(map[cityName].min, measurement);
      map[cityName].max = maxBigInt(map[cityName].max, measurement);
      map[cityName].total += measurement;
      map[cityName].count++;
    } else {
      map[cityName] = {
        min: measurement,
        max: measurement,
        total: measurement,
        count: BigInt(1),
      };
    }
  }

  let bucket = "";
  const list = Object.entries(map);
  list.sort((l, r) => {
    if (l[0] < r[0]) {
      return -1;
    } else if (l[0] > r[0]) {
      return 1;
    } else {
      return 0;
    }
  });
  for (const [city_name, status] of list) {
    const avg = status.total / status.count;
    const line = `${city_name}=${status.min};${status.max};${avg}(${status.total}/${status.count})\n`;

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
