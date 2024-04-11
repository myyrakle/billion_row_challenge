const fs = require("fs");
const readline = require("readline");
const { OUTPUT_PATH, MEASUREMENTS_PATH, Timer } = require("./common");

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
      min: BigInt(9007199254740991),
      max: BigInt(0),
      total: BigInt(0),
      count: BigInt(0),
    },
  ])
);
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

  for await (const line of rl) {
    let [cityName, measurement] = line.split(";");
    measurement = BigInt(measurement);

    const city = cityMap.get(cityName);
    city.min = minBigInt(city.min, measurement);
    city.max = maxBigInt(city.max, measurement);
    city.total += measurement;
    city.count++;
  }

  let bucket = "";

  for (const [city_name, status] of Array.from(cityMap)) {
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
