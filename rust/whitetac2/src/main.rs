pub mod common;
use common::Timer;
use hashbrown::HashMap;
use memmap2::Mmap;
use phf::phf_map;
use rayon::prelude::*;
use std::fs::File;
use std::mem::ManuallyDrop;
use std::sync::{Arc, Mutex};
use std::{mem, str};

#[derive(Debug, Clone)]
struct Status {
    min: i64,
    max: i64,
    total: i64,
    count: i64,
}
#[inline(always)]
fn parse_to_i64(v: &[u8], size: usize) -> i64 {
    let mut result = 0i64;
    //let mut negative = false;
    let i = 0;

    // if v.len() > 0 && v[0] == b'-' {
    //     negative = true;
    //     i = 1;
    // }

    for &byte in &v[i..size] {
        let digit = (byte - b'0') as i64;
        result = result.wrapping_mul(10) + digit;
    }
    // if result.to_string() != String::from_utf8(v.clone()).unwrap() {
    //     println!("{:?} {:?}", result, String::from_utf8(v.clone()).unwrap());
    // }
    // if negative {
    //     -result
    // } else {
    result
    //}
}

fn city_hash(city: &[u8]) -> u64 {
    let mut hash: u64 = 0;
    let len = city.len();
    for i in 0..len {
        if i >= 7 {
            break;
        }
        hash += (city[i] as u64).wrapping_shl((i * 8) as u32);
    }
    hash
}
#[inline(always)]
fn solution(path: &str) -> String {
    let city_names: phf::Map<u64, &'static str> = phf_map! {
       29680635344151617u64 => "Adenarith",
    28273277742050625u64 => "Amsterdam",
    30521827025710657u64 => "Anápolis",
    29664159916453953u64 => "Aparecida de Goiânia",
    126917984941121u64 => "Athens",
    121399204345153u64 => "Austin",
    31078114690359618u64 => "Bahrain",
    31362888017797442u64 => "Bangalore",
    31362905130819906u64 => "Barcelona",
    31323026560410946u64 => "Belo Horizonte",
    120575895561538u64 => "Belém",
    32485440953085762u64 => "Boa Vista",
    121424974147394u64 => "Boston",
    30590352450744898u64 => "Brasília",
    30510844017472066u64 => "Brussels",
    28554735336518978u64 => "Bucharest",
    27424471877771587u64 => "Campinas",
    107083759247683u64 => "Canada",
    30506441692308803u64 => "Central",
    29661999481055299u64 => "Chennai",
    29679561602984003u64 => "Chongqing",
    27417896098492227u64 => "Copenhagen",
    45532298558928195u64 => "Cuiabá",
    27700496423875907u64 => "Curitiba",
    126862268064068u64 => "Dallas",
    121399069013316u64 => "Dublin",
    28183117805221188u64 => "Duque de Caxias",
    28183100574491974u64 => "Feira de Santana",
    28548138469060422u64 => "Fortaleza",
    33045183874560582u64 => "Frankfurt",
    32485519604670791u64 => "Gaaphis",
    27418995542552391u64 => "Goiania",
    27422238292669767u64 => "Guadalajara",
    29392649263871303u64 => "Guarulhos",
    30239221695145288u64 => "Helsinki",
    31326324961210184u64 => "Hong Kong",
    27691691672893768u64 => "Hyderabad",
    27424437400006217u64 => "Indianapolis",
    28268862380536393u64 => "Ireland",
    33040798496093001u64 => "Istanbul",
    28539063303238986u64 => "Juiz de Fora",
    1986357579u64 => "Kiev",
    27431034503851851u64 => "Kolkata",
    32777959466037835u64 => "Krofast",
    435711603275u64 => "Krore",
    32777959466230092u64 => "Larfast",
    121424705384268u64 => "London",
    31078187470319436u64 => "Londrina",
    29113148597563212u64 => "Los Angeles",
    45532358688072013u64 => "Macapá",
    110404053524813u64 => "Madrid",
    126947982991693u64 => "Manaus",
    9129672016684365u64 => "Mexico City",
    452806666573u64 => "Miami",
    474080897357u64 => "Milan",
    27414614995005261u64 => "Montreal",
    131320293584717u64 => "Moscow",
    115866984084813u64 => "Mumbai",
    29674037548428878u64 => "N. California",
    29117719343279694u64 => "N. Virginia",
    30510640761627982u64 => "New Delhi",
    32210575932482894u64 => "New York",
    50598916927023438u64 => "Niterói",
    29072326024130382u64 => "Nova Iguaçu",
    1869178959u64 => "Ohio",
    121424755126863u64 => "Oregon",
    418413376335u64 => "Osaka",
    122472928211791u64 => "Osasco",
    1869378383u64 => "Oslo",
    126862284841296u64 => "Palmas",
    495690342736u64 => "Paris",
    18331536553307984u64 => "Porto Alegre",
    24242511064231760u64 => "Porto Velho",
    478409224784u64 => "Prico",
    125780104278608u64 => "Prover",
    1701737808u64 => "Pune",
    114793359176273u64 => "Qreigh",
    31366281109140049u64 => "Qrokwood",
    111490529191250u64 => "Recife",
    55013417456658770u64 => "Ribeirão Preto",
    9118679970048338u64 => "Rio de Janeiro",
    31354092176302419u64 => "Salvador",
    18331536553042259u64 => "Santo André",
    33039569044595027u64 => "Sao Paulo",
    465826702675u64 => "Seoul",
    31367286064310611u64 => "Singapore",
    32762492110140499u64 => "St. Petersburg",
    31358532853920851u64 => "Stockholm",
    133476550736211u64 => "Sydney",
    28501679727166291u64 => "São Bernardo do Campo",
    31321927052411731u64 => "São Gonçalo",
    31325225587295059u64 => "São José dos Campos",
    27391172983112531u64 => "São Paulo",
    478778453844u64 => "Tokyo",
    31371740079877972u64 => "Toronto",
    121399203557973u64 => "Urgtin",
    33343168521855318u64 => "Vancouver",
    107126926436694u64 => "Vienna",
    131260432408919u64 => "Warsaw",
    114776180159834u64 => "Zurich",
        };
    let mut pre_map = HashMap::new();
    for city_code in city_names.keys() {
        pre_map.insert(
            *city_code,
            Status {
                min: i64::MAX,
                max: i64::MIN,
                total: 0,
                count: 0,
            },
        );
    }

    let core = num_cpus::get();
    let file = File::open(path).unwrap();
    let mmap: Mmap = unsafe { Mmap::map(&file).unwrap() };
    let maps: Arc<Mutex<Vec<Box<HashMap<u64, Status>>>>> =
        Arc::new(Mutex::new(Vec::with_capacity(core + 1)));
    let mut index: usize;
    //let max_index = 1000000000;

    //let start = Instant::now(); // 시작 시간 기록
    let chunk_size = mmap.len() / core;
    // mmap을 core수만큼 나누어서 병렬처리 \n이 마지막에 있도록 분할
    let mut chunks_start_indexes: Vec<usize> = Vec::with_capacity(core + 1);
    chunks_start_indexes.push(0);
    index = chunk_size;
    loop {
        if mmap[index] == b'\n' {
            index += 1;
            chunks_start_indexes.push(index);
            index += chunk_size;
        } else {
            index += 1;
        }
        if index >= mmap.len() {
            chunks_start_indexes.push(mmap.len());
            break;
        }
    }
    let mut buffers: Vec<ManuallyDrop<&[u8]>> = Vec::new();
    for chunks_start_index in 0..chunks_start_indexes.len() - 1 {
        let chunk_start = chunks_start_indexes[chunks_start_index];
        let chunk_end = chunks_start_indexes[chunks_start_index + 1];
        buffers.push(ManuallyDrop::new(mmap[chunk_start..chunk_end].as_ref()));
    }
    buffers.par_iter().for_each(|full_buffer| {
        let mut map1: HashMap<u64, Status> = pre_map.clone();
        //map1을 phf맵을 보고 미리 할당
        for buffer in full_buffer.split(|c| *c == b'\n') {
            let mut city_name: [u8; 100] = [0; 100];
            let mut city_name_size = 0;
            let mut buffer_index = 0;
            while let Some(&b) = buffer.get(buffer_index) {
                if b == b';' {
                    break;
                }
                city_name[city_name_size] = b;
                city_name_size += 1;
                buffer_index += 1;
            }
            if buffer.len() <= 0 {
                continue;
            }
            buffer_index += 1; // skip ;
            let mut measurement_vec: [u8; 100] = [0; 100];
            let mut measurement_vec_size = 0;
            while let Some(&b) = buffer.get(buffer_index) {
                if b == b'\n' {
                    break;
                }
                measurement_vec[measurement_vec_size] = b;
                measurement_vec_size += 1;
                buffer_index += 1;
            }
            let measurement: i64 = parse_to_i64(&measurement_vec, measurement_vec_size);
            let city_code = city_hash(&city_name);

            /*let status = map1.entry(city_code.clone()).or_insert_with(|| Status {
                min: measurement,
                max: measurement,
                total: 0,
                count: 0,
            });*/
            let status = unsafe { map1.get_mut(&city_code).unwrap_unchecked() };
            status.min = status.min.min(measurement);
            status.max = status.max.max(measurement);
            status.total += measurement;
            status.count += 1;
        }
        unsafe { maps.lock().unwrap_unchecked().push(Box::new(map1)) };
    });

    let unlock_maps = maps.lock().unwrap();
    let mut map = HashMap::new();
    for m in unlock_maps.iter() {
        for (city_code, status) in m.iter() {
            let status2 = map.entry(*city_code).or_insert_with(|| Status {
                min: status.min,
                max: status.max,
                total: 0,
                count: 0,
            });
            status2.min = status.min.min(status2.min);
            status2.max = status.max.max(status2.max);
            status2.total += status.total;
            status2.count += status.count;
        }
    }

    let mut bucket = String::new();
    let mut list = map.iter().collect::<Vec<_>>();
    list.sort_by(|a, b| {
        city_names
            .get(a.0)
            .unwrap()
            .cmp(city_names.get(b.0).unwrap())
    });
    for (city_code, status) in list {
        let avg = status.total / status.count;
        let line = format!(
            "{}={};{};{}({}/{})\n",
            city_names.get(city_code).unwrap(),
            status.min,
            status.max,
            avg,
            status.total,
            status.count,
        );
        bucket.push_str(&line);
    }
    mem::forget(city_names);
    mem::forget(unlock_maps);
    mem::forget(mmap);
    bucket
}

fn main() {
    let expect_output = std::fs::read_to_string(common::OUTPUT_PATH).unwrap();

    let timer = Timer::new();
    let got = solution(common::MEASUREMENTS_PATH);
    println!("Elapsed: {}ms", timer.elapsed_as_millis());

    assert_eq!(expect_output, got);
}
