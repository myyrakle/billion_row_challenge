pub mod common;
use std::{fs::File, ops::Range, sync::{mpsc::{self, Sender}, Arc}};

use common::Timer;
use memmap2::Mmap;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

const MAP_SIZE: usize = 511;

struct Status {
    city_name: [u8; 24],
    city_name_start: usize,
    min: i64,
    max: i64,
    total: i64,
    count: i64,
}

impl Status {
    fn new() -> Self {
        Self { city_name: [0; 24], city_name_start: 0, min: i64::MAX, max: 0, total: 0, count: 0 }
    }
}

fn worker(contents: &[u8], sender: &mut Sender<Vec<Status>>) {
    let mut index: usize = 0;
    let mut status_map = Vec::from_iter((0..511).map(|_| Status::new()));
    let mut char;

    while index < contents.len() {
        let mut hash: u32 = 19000;

        index += 2;

        // 이름 해싱
        while {
            char = contents[index];
            char != b';'
        } {
            hash = hash.wrapping_shl(5) + hash + char as u32;
            index += 1;
        }
        index += 1;

        let mut measurement: i64 = 0;
        while {
            char = contents[index];
            char != b'\n'
        } {
            measurement = measurement * 10 + (char - b'0') as i64;
            index += 1;
        }
        index += 1;

        let key = hash as usize % MAP_SIZE;
        let status = &mut status_map[key];

        if status.count == 0 {
            status.city_name_start = 23;
            let mut index = index;
            while contents[index] != b';' { index -= 1; }
            index = index.wrapping_sub(1);
            while index != usize::MAX && contents[index] != b'\n' {
                status.city_name[status.city_name_start] = contents[index];
                status.city_name_start -= 1;
                index -= 1;
            }
            status.city_name_start += 1;
        }

        status.min = status.min.min(measurement);
        status.max = status.max.max(measurement);
        status.total += measurement;
        status.count += 1;
    }

    sender.send(status_map).unwrap();
}

fn solution(path: &str) -> String {
    let num_cpus: usize = num_cpus::get();

    // 파일 읽기
    let mmap = {
        let file = File::open(path).unwrap();
        let mmap = unsafe { Mmap::map(&file).unwrap() };
        Arc::new(mmap)
    };

    // 내용을 나눔
    let ranges = {
        let contents = &mmap[..];

        let mut ranges: Vec<Range<usize>> = Vec::with_capacity(num_cpus);
        let chunk_size = contents.len() / num_cpus;
        let mut start = 0;
        let mut end = 0;

        loop {
            end = start + chunk_size;

            if end >= contents.len() {
                end = contents.len();
            } else {
                while contents[end] != b'\n' { end += 1; }
                end += 1;
            }

            ranges.push(start..end);

            if end >= contents.len() { break }

            start = end;
        }

        ranges
    };
    
    // 스레드 실행
    let rx = {
        let (tx, rx) = mpsc::channel::<Vec<Status>>();
        ranges.par_iter().for_each_with(tx, |tx, range| {
            worker(&Arc::clone(&mmap)[range.clone()], tx);
        });
        rx
    };

    let mut city_name_and_keys = Vec::with_capacity(100);

    let mut map = Vec::from_iter((0..511).map(|_| Status::new()));

    for status_map in rx.iter() {
        if city_name_and_keys.is_empty() {
            for index in 0..status_map.len() {
                if status_map[index].count != 0 {
                    city_name_and_keys.push((
                        (status_map[index].city_name, status_map[index].city_name_start),
                        index
                    ));
                }
            }
            city_name_and_keys.sort_by(|((name, start), _), ((name2, start2), _)|
                name[*start..].cmp(&name2[*start2..])
            );
        }

        for (_, key) in &city_name_and_keys {
            let status = &mut map[*key];
            let status2 = &status_map[*key];

            status.min = status.min.min(status2.min);
            status.max = status.max.max(status2.max);
            status.total += status2.total;
            status.count += status2.count;
        }
    }

    let mut result = String::new();
    for ((name, start), key) in &city_name_and_keys {
        let status = &map[*key];

        let city_name = unsafe { std::str::from_utf8_unchecked(&name[*start..]) };
        let min = status.min;
        let max = status.max;
        let total = status.total;
        let count = status.count;
        let average = total / count;
    
        result = result + &format!("{city_name}={min};{max};{average}({total}/{count})\n");
    }

    result
}

fn main() {
    let expect_output = std::fs::read_to_string(common::OUTPUT_PATH).unwrap();

    let timer = Timer::new();
    let got = solution(common::MEASUREMENTS_PATH);
    println!("Elapsed: {}ms", timer.elapsed_as_millis());

    assert_eq!(expect_output, got);
}