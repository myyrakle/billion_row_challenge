use fnv::FnvHasher;
use phf::phf_map;
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::{fs, time::Instant};

type FnvHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FnvHasher>>;

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
    for &byte in &v[..size] {
        let digit = (byte - b'0') as i64;
        result = result.wrapping_mul(10) + digit;
    }
    result
}

#[inline(always)]
fn city_hash(city: &[u8]) -> u64 {
    let mut hasher = FnvHasher::default();
    hasher.write(city);
    hasher.finish()
}

#[inline(always)]
fn solution(path: &str) -> String {
    let city_names: phf::Map<u64, &'static str> = phf_map! {
        // ... (city names omitted for brevity)
    };

    let mut pre_map = FnvHashMap::with_capacity_and_hasher(100, Default::default());
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

    // ... (remaining code omitted for brevity)

    let mut bucket = String::new();
    let mut list = pre_map.iter().collect::<Vec<_>>();
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

    bucket
}

fn main() {
    let expect_output = fs::read_to_string("output.txt").unwrap();

    let start = Instant::now();
    let got = solution("measurements.txt");
    let elapsed = start.elapsed();
    println!("Elapsed: {:?}", elapsed);

    assert_eq!(expect_output, got);
}
