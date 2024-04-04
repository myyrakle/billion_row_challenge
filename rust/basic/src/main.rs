pub mod common;
use common::Timer;

fn solution(path: &str) -> String {
    use std::collections::HashMap;
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);

    let mut map = HashMap::new();
    struct Status {
        min: i64,
        max: i64,
        total: i64,
        count: i64,
    }

    for line_result in reader.lines() {
        let line = line_result.unwrap();

        let mut parts = line.split(';');
        let city_name = parts.next().unwrap().to_string();
        let measurement = parts.next().unwrap().parse::<i64>().unwrap();

        if let Some(Status {
            min,
            max,
            total,
            count,
        }) = map.get_mut(&city_name)
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
    }

    let mut bucket = String::new();
    let mut list = map.into_iter().collect::<Vec<_>>();
    list.sort_by(|a, b| a.0.cmp(&b.0));
    for (city_name, status) in list {
        let avg = status.total / status.count;
        let line = format!(
            "{}={};{};{}({}/{})\n",
            city_name, status.min, status.max, avg, status.total, status.count,
        );
        bucket.push_str(&line);
    }

    bucket
}

fn main() {
    let expect_output = std::fs::read_to_string(common::OUTPUT_PATH).unwrap();

    let timer = Timer::new();
    let got = solution(common::MEASUREMENTS_PATH);
    println!("Elapsed: {}ms", timer.elapsed_as_millis());

    assert_eq!(expect_output, got);
}
