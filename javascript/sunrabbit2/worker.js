const fs = require("fs");
const readline = require("readline");
const { parentPort, workerData } = require("node:worker_threads");
// const workerData = {
//   path: "measurements.txt",
//   start: 0,
//   end: 1142364117,
// };

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
      total: 0,
      count: 0,
    },
  ])
);

async function processChunk() {
  const { path, start, end } = workerData;
  if (start > end - 1) {
    return new Map();
  }

  const readStream = fs.createReadStream(path, {
    start: start,
    end: end - 1,
  });

  const rl = readline.createInterface({
    input: readStream,
    crlfDelay: Infinity,
  });

  rl.addListener("line", (line) => {
    let [cityName, measurement] = line.split(";");

    measurement = Number(measurement);

    const city = cityMap.get(cityName);
    city.min = Math.min(city.min, measurement);
    city.max = Math.max(city.max, measurement);
    city.total += measurement;
    city.count += 1;
  });

  await new Promise((resolve, _) => {
    rl.addListener("close", resolve);
  });

  return cityMap;
}

processChunk().then((v) => {
  parentPort.postMessage(v);
});
