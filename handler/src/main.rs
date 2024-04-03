use std::{collections::HashMap, fs::OpenOptions, io::Write};

const CITY_NAMES: [&'static str; 106] = [
    "Seoul",
    "Tokyo",
    "Singapore",
    "Sydney",
    "Mumbai",
    "Frankfurt",
    "London",
    "Ireland",
    "Sao Paulo",
    "N. Virginia",
    "Ohio",
    "N. California",
    "Oregon",
    "Canada",
    "Central",
    "Paris",
    "Stockholm",
    "Bahrain",
    "Hong Kong",
    "Osaka",
    "Krofast",
    "Prover",
    "Qrokwood",
    "Larfast",
    "Gaaphis",
    "Qreigh",
    "Prico",
    "Krore",
    "Urgtin",
    "Adenarith",
    "New York",
    "Los Angeles",
    "Dallas",
    "Miami",
    "Mexico City",
    "Sao Paulo",
    "Montreal",
    "Vancouver",
    "London",
    "Paris",
    "Stockholm",
    "Frankfurt",
    "Milan",
    "Madrid",
    "Warsaw",
    "Dublin",
    "Brussels",
    "Amsterdam",
    "Zurich",
    "Oslo",
    "Helsinki",
    "Copenhagen",
    "Vienna",
    "Athens",
    "Bucharest",
    "Istanbul",
    "Moscow",
    "St. Petersburg",
    "Kiev",
    "Mumbai",
    "New Delhi",
    "Bangalore",
    "Hyderabad",
    "Chennai",
    "Kolkata",
    "Pune",
    "São Paulo",
    "Rio de Janeiro",
    "Belo Horizonte",
    "Brasília",
    "Salvador",
    "Fortaleza",
    "Manaus",
    "Curitiba",
    "Recife",
    "Porto Alegre",
    "Belém",
    "Goiania",
    "Guarulhos",
    "Campinas",
    "Nova Iguaçu",
    "Santo André",
    "São Bernardo do Campo",
    "Osasco",
    "Duque de Caxias",
    "São José dos Campos",
    "Ribeirão Preto",
    "Niterói",
    "São Gonçalo",
    "Feira de Santana",
    "Juiz de Fora",
    "Aparecida de Goiânia",
    "Londrina",
    "Anápolis",
    "Porto Velho",
    "Cuiabá",
    "Macapá",
    "Palmas",
    "Boa Vista",
    "Austin",
    "Boston",
    "Indianapolis",
    "Chongqing",
    "Guadalajara",
    "Barcelona",
    "Toronto",
];

fn get_city_name() -> &'static str {
    use rand::seq::SliceRandom;
    use rand::thread_rng;

    let mut rng = thread_rng();
    let random_city = CITY_NAMES.choose(&mut rng);

    random_city.unwrap()
}

// 0.0 ~ 1000000.0 (1자리까지 반올림)
fn generate_measurement() -> f64 {
    use rand::Rng;

    let mut rng = rand::thread_rng();
    let measurement = rng.gen_range(0.0..1000000.0);

    let rounded_measurement = (measurement * 10.0_f64).round() / 10.0;

    rounded_measurement
}

pub struct Timer {
    start: std::time::Instant,
}

impl Timer {
    pub fn new() -> Timer {
        Timer {
            start: std::time::Instant::now(),
        }
    }

    pub fn elapsed(&self) -> std::time::Duration {
        self.start.elapsed()
    }

    pub fn elapsed_as_millis(&self) -> u128 {
        self.elapsed().as_millis()
    }

    pub fn elapsed_as_secs(&self) -> u64 {
        self.elapsed().as_secs()
    }
}

fn main() {
    let mut measurements_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("measurements.txt")
        .unwrap();

    let mut outputs_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("outputs.txt")
        .unwrap();

    let mut map = HashMap::new();
    struct Status {
        min: f64,
        max: f64,
        total: f64,
        count: i128,
    }

    let timer = Timer::new();

    let mut bucket = String::new();
    let mut bucket_count = 0;

    for _ in 0..10_0000_0000_i128 {
        let city_name = get_city_name();
        let measurement = generate_measurement();

        if let Some(Status {
            min,
            max,
            total,
            count,
        }) = map.get_mut(city_name)
        {
            *min = min.min(measurement);
            *max = max.max(measurement);
            *total += measurement;
            *count += 1;
        } else {
            map.insert(
                city_name,
                Status {
                    min: measurement,
                    max: measurement,
                    total: measurement,
                    count: 1,
                },
            );
        }

        let line = format!("{city_name};{measurement}\n");
        bucket.push_str(&line);
        bucket_count += 1;

        if bucket_count == 10000000 {
            let buffer = bucket.as_bytes();
            measurements_file.write(buffer).unwrap();
            bucket.clear();
            bucket_count = 0;
        }
    }

    let elapsed_secs = timer.elapsed_as_secs();
    println!("measurements generate Done! {}s", elapsed_secs);

    let timer = Timer::new();
    let mut list = map.into_iter().collect::<Vec<_>>();
    list.sort_by(|a, b| a.0.cmp(&b.0));
    for (city_name, status) in list {
        let avg = status.total / status.count as f64;
        let line = format!(
            "{}={:.1};{:.1};{:.1}({:.1}/{})\n",
            city_name, status.min, status.max, avg, status.total, status.count,
        );
        let buffer = line.as_bytes();
        outputs_file.write(buffer).unwrap();
    }

    let elapsed_secs = timer.elapsed_as_secs();
    println!("outputs generate Done! {}s", elapsed_secs);
}
