const fs = require("node:fs");
const fsp = require("node:fs/promises");
const os = require("node:os");
const workerThreads = require("node:worker_threads");

const { OUTPUT_PATH, MEASUREMENTS_PATH, Timer } = require("../common");

const cityMap = new Map(
  [
    "Adenarith",
    "Amsterdam",
    "Anápolis",
    "Aparecida de Goiânia",
    "Athens",
    "Austin",
    "Bahrain",
    "Bangalore",
    "Barcelona",
    "Belo Horizonte",
    "Belém",
    "Boa Vista",
    "Boston",
    "Brasília",
    "Brussels",
    "Bucharest",
    "Campinas",
    "Canada",
    "Central",
    "Chennai",
    "Chongqing",
    "Copenhagen",
    "Cuiabá",
    "Curitiba",
    "Dallas",
    "Dublin",
    "Duque de Caxias",
    "Feira de Santana",
    "Fortaleza",
    "Frankfurt",
    "Gaaphis",
    "Goiania",
    "Guadalajara",
    "Guarulhos",
    "Helsinki",
    "Hong Kong",
    "Hyderabad",
    "Indianapolis",
    "Ireland",
    "Istanbul",
    "Juiz de Fora",
    "Kiev",
    "Kolkata",
    "Krofast",
    "Krore",
    "Larfast",
    "London",
    "Londrina",
    "Los Angeles",
    "Macapá",
    "Madrid",
    "Manaus",
    "Mexico City",
    "Miami",
    "Milan",
    "Montreal",
    "Moscow",
    "Mumbai",
    "N. California",
    "N. Virginia",
    "New Delhi",
    "New York",
    "Niterói",
    "Nova Iguaçu",
    "Ohio",
    "Oregon",
    "Osaka",
    "Osasco",
    "Oslo",
    "Palmas",
    "Paris",
    "Porto Alegre",
    "Porto Velho",
    "Prico",
    "Prover",
    "Pune",
    "Qreigh",
    "Qrokwood",
    "Recife",
    "Ribeirão Preto",
    "Rio de Janeiro",
    "Salvador",
    "Santo André",
    "Sao Paulo",
    "Seoul",
    "Singapore",
    "St. Petersburg",
    "Stockholm",
    "Sydney",
    "São Bernardo do Campo",
    "São Gonçalo",
    "São José dos Campos",
    "São Paulo",
    "Tokyo",
    "Toronto",
    "Urgtin",
    "Vancouver",
    "Vienna",
    "Warsaw",
    "Zurich",
  ].map((v) => [
    v,
    {
      min: Number.MAX_SAFE_INTEGER,
      max: 0,
      total: 0,
      count: 0,
    },
  ])
);

const BUFFER_SIZE = 128;
async function solution() {
  const file = await fsp.open(MEASUREMENTS_PATH);
  const size = (await file.stat()).size;
  const workerCount = os.cpus().length - 1;
  const chunkSize = ~~(size / workerCount); // fast Math.floor
  const threadHeapSize = 1024;

  const offsets = [];
  const buffer = Buffer.alloc(BUFFER_SIZE);
  let offset = 0;

  while (true) {
    offset += chunkSize;

    if (offset >= size || offset < 0) {
      offsets.push(size);
      break;
    }

    await file.read(buffer, 0, BUFFER_SIZE, offset);

    const pos = buffer.indexOf(10); // \n
    buffer.fill(0);

    if (pos !== -1) {
      offset += pos + 1;
      offsets.push(offset);
    } else {
      offsets.push(size);
      break;
    }
  }
  file.close();

  await Promise.all(
    offsets.map(async (v, idx, array) => {
      return new Promise((resolve, _) => {
        const worker = new workerThreads.Worker(
          "./nodejs/sunrabbit2/worker.js",
          {
            workerData: {
              path: MEASUREMENTS_PATH,
              start: idx === 0 ? 0 : array[idx - 1],
              end: v,
            },
            resourceLimits: {
              maxOldGenerationSizeMb: threadHeapSize,
            },
          }
        );

        worker.on("message", (message) => {
          for (const [key, value] of message.entries()) {
            const existing = cityMap.get(key);
            existing.min = Math.min(existing.min, value.min);
            existing.max = Math.max(existing.max, value.max);
            existing.total += value.total;
            existing.count += value.count;
          }
        });

        worker.on("error", (err) => {
          console.error(err);
        });

        worker.on("exit", (code) => {
          if (code !== 0) {
            new Error(`Worker stopped with exit code ${code}`);
          }
          resolve();
        });
      });
    })
  );
  return Array.from(cityMap)
    .map(
      ([city_name, status]) =>
        `${city_name}=${status.min};${status.max};${~~(
          status.total / status.count
        )}(${status.total}/${status.count})\n`
    )
    .join("");
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
