const fs = require("node:fs");
const readline = require("readline");

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
      min: 9007199254740991,
      max: 0,
      total: BigInt(0),
      count: 0,
    },
  ])
);

async function solution() {
  const instream = fs.createReadStream(MEASUREMENTS_PATH);
  const rl = readline.createInterface({
    input: instream,
    crlfDelay: Infinity,
  });
  rl.addListener("line", (line) => {
    let [cityName, measurement] = line.split(";");

    measurement = Number(measurement);

    const city = cityMap.get(cityName);
    city.min = Math.min(city.min, measurement);
    city.max = Math.max(city.max, measurement);
    city.total += BigInt(measurement);
    city.count++;
  });

  await new Promise((resolve, _) => {
    rl.addListener("close", () => resolve());
  });

  return Array.from(cityMap)
    .map(
      ([city_name, status]) =>
        `${city_name}=${status.min};${status.max};${~~(
          status.total / BigInt(status.count)
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
