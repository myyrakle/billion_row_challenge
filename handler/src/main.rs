use std::{collections::HashMap, fs::OpenOptions, io::Write, io::BufWriter};

const CITY_NAMES: [&'static str; 100] = [
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
	"Zurich"
];

fn get_city_name() -> &'static str {
    use rand::seq::SliceRandom;
    use rand::thread_rng;

    let mut rng = thread_rng();
    let random_city = CITY_NAMES.choose(&mut rng);

    random_city.unwrap()
}

// 0.0 ~ 1000000.0 (1자리까지 반올림)
fn generate_measurement() -> i64 {
    use rand::Rng;

    let mut rng = rand::thread_rng();
    let measurement = rng.gen_range(0..50000000);

    measurement
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
    let measurements_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("measurements.txt")
        .unwrap();
    let mut measurements_writer = BufWriter::new(measurements_file);

    let outputs_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("outputs.txt")
        .unwrap();
    let mut outputs_writer = BufWriter::new(outputs_file);

    let mut map = HashMap::new();
    struct Status {
        min: i64,
        max: i64,
        total: i64,
        count: i64,
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
            *min = (*min).min(measurement);
            *max = (*max).max(measurement);
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
            measurements_writer.write(bucket.as_bytes()).unwrap();
            bucket.clear();
            bucket_count = 0;
        }
    }
    measurements_writer.flush().unwrap();

    let elapsed_secs = timer.elapsed_as_secs();
    println!("measurements generate Done! {}s", elapsed_secs);

    let timer = Timer::new();
    let mut list = map.into_iter().collect::<Vec<_>>();
    list.sort_by(|a, b| a.0.cmp(&b.0));
    for (city_name, status) in list {
        let avg = status.total / status.count;
        let line = format!(
            "{}={};{};{}({}/{})\n",
            city_name, status.min, status.max, avg, status.total, status.count,
        );
        outputs_writer.write(line.as_bytes()).unwrap();
    }
    outputs_writer.flush().unwrap();

    let elapsed_secs = timer.elapsed_as_secs();
    println!("outputs generate Done! {}s", elapsed_secs);
}
