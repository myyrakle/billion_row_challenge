#![feature(portable_simd)]

pub mod common;

use common::Timer;
use memmap2::{Advice, Mmap};
use std::fmt::Write;
use std::fs::File;
use std::mem::ManuallyDrop;
use std::simd::prelude::*;

const HASH_BITS: u32 = 9;
const HASH_SIZE: usize = 1 << HASH_BITS;
const HASH_MULT: u64 = 0xe283_4894_2a2f_5c93;

#[derive(Clone, Copy)]
struct Slot {
    total: i64,
    count: u32,
    min: i32,
    max: i32,
}

const EMPTY_SLOT: Slot = Slot {
    total: 0,
    count: 0,
    min: i32::MAX,
    max: -1,
};

const CITY_NAMES: [&str; 100] = [
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
];

type Table = [Slot; HASH_SIZE];

#[inline(always)]
fn hash_word(word: u64) -> usize {
    (word.wrapping_mul(HASH_MULT) >> (64 - HASH_BITS)) as usize
}

#[inline(always)]
unsafe fn load_u64(position: *const u8) -> u64 {
    u64::from_le((position as *const u64).read_unaligned())
}

#[inline(always)]
fn update_slot(table: &mut Table, word: u64, value: i32) {
    let slot = unsafe { table.get_unchecked_mut(hash_word(word)) };
    slot.count += 1;
    slot.total += value as i64;
    slot.min = slot.min.min(value);
    slot.max = slot.max.max(value);
}

#[inline(always)]
fn parse_digits_swar(word: u64, digit_len: usize) -> i32 {
    let shifted = word.wrapping_sub(0x3030_3030_3030_3030) << ((8 - digit_len) * 8);
    let pairs = (shifted & 0x000f_000f_000f_000f).wrapping_mul(10)
        + ((shifted >> 8) & 0x000f_000f_000f_000f);
    let quads = (pairs & 0x0000_ffff_0000_ffff).wrapping_mul(100)
        + ((pairs >> 16) & 0x0000_ffff_0000_ffff);
    ((quads & 0xffff_ffff).wrapping_mul(10_000) + (quads >> 32)) as i32
}

#[inline(always)]
unsafe fn scan_record_simd(position: *const u8) -> (u64, usize, usize) {
    let bytes = u8x32::from_slice(std::slice::from_raw_parts(position, 32));
    let words: u64x4 = core::mem::transmute(bytes);
    let semicolons = bytes.simd_eq(u8x32::splat(b';')).to_bitmask();
    let newlines = bytes.simd_eq(u8x32::splat(b'\n')).to_bitmask();
    debug_assert!(semicolons != 0 && newlines != 0);
    (
        words[0],
        semicolons.trailing_zeros() as usize,
        newlines.trailing_zeros() as usize + 1,
    )
}

fn worker_scalar(data: &[u8], start: usize, end: usize, table: &mut Table) {
    let mut position = start;
    while position < end {
        let name_off = position;
        let mut name_len = 0usize;
        while position + name_len < end && data[position + name_len] != b';' {
            name_len += 1;
        }
        if position + name_len >= end {
            break;
        }

        let mut word = 0u64;
        for (index, &byte) in data[name_off..name_off + name_len.min(8)].iter().enumerate() {
            word |= (byte as u64) << (index * 8);
        }
        position += name_len + 1;

        let mut value = 0i32;
        while position < end && data[position] != b'\n' {
            value = value * 10 + (data[position] - b'0') as i32;
            position += 1;
        }
        if position < end {
            position += 1;
        }
        update_slot(table, word, value);
    }
}

const LANES: usize = 3;

const PREFETCH_AHEAD: usize = 256;

fn worker_ilp(data: &[u8], start: usize, end: usize, table: &mut Table) {
    let span = end - start;
    if span < 4096 {
        worker_scalar(data, start, end, table);
        return;
    }

    let data_start = data.as_ptr();
    let data_fast_end = unsafe { data_start.add(data.len().saturating_sub(64)) };
    let mut positions = [data_start; LANES];
    let mut ends = [data_start; LANES];
    positions[0] = unsafe { data_start.add(start) };
    for lane in 1..LANES {
        let mut boundary = start + span * lane / LANES;
        while data[boundary] != b'\n' {
            boundary += 1;
        }
        let lane_start = unsafe { data_start.add(boundary + 1) };
        positions[lane] = lane_start;
        ends[lane - 1] = lane_start;
    }
    ends[LANES - 1] = unsafe { data_start.add(end) };

    'outer: loop {
        for lane in 0..LANES {
            if positions[lane] >= ends[lane] || positions[lane] >= data_fast_end {
                break 'outer;
            }
        }
        for lane in 0..LANES {
            let position = positions[lane];
            unsafe {
                core::arch::x86_64::_mm_prefetch(
                    position.wrapping_add(PREFETCH_AHEAD) as *const i8,
                    core::arch::x86_64::_MM_HINT_T0,
                );
            }
            let (word_raw, name_len, record_len) = unsafe { scan_record_simd(position) };
            let word = unsafe {
                core::arch::x86_64::_bzhi_u64(word_raw, (name_len * 8) as u32)
            };
            let value = parse_digits_swar(
                unsafe { load_u64(position.add(name_len + 1)) },
                record_len - name_len - 2,
            );
            positions[lane] = unsafe { position.add(record_len) };
            update_slot(table, word, value);
        }
    }

    for lane in 0..LANES {
        let position = unsafe { positions[lane].offset_from(data_start) as usize };
        let lane_end = unsafe { ends[lane].offset_from(data_start) as usize };
        worker_scalar(data, position, lane_end, table);
    }
}

fn solution(path: &str) -> String {
    let file = File::open(path).unwrap();
    let mmap = ManuallyDrop::new(unsafe { Mmap::map(&file).unwrap() });
    let _ = mmap.advise(Advice::Sequential);
    let _ = mmap.advise(Advice::WillNeed);
    let _ = mmap.advise(Advice::HugePage);

    let data: &[u8] = &mmap;
    let len = data.len();
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let chunk = (len / threads).max(1);

    let mut tables: Vec<Box<Table>> = Vec::with_capacity(threads);
    std::thread::scope(|scope| {
        let mut handles = Vec::with_capacity(threads);
        let mut start = 0usize;
        for _ in 0..threads {
            if start >= len {
                break;
            }
            let mut end = start + chunk;
            if end >= len {
                end = len;
            } else {
                while data[end] != b'\n' {
                    end += 1;
                }
                end += 1;
            }
            handles.push(scope.spawn(move || {
                let mut table = Box::new([EMPTY_SLOT; HASH_SIZE]);
                worker_ilp(data, start, end, &mut table);
                table
            }));
            start = end;
        }
        for handle in handles {
            tables.push(handle.join().unwrap());
        }
    });

    let mut merged = Box::new([EMPTY_SLOT; HASH_SIZE]);
    for table in &tables {
        for (destination, source) in merged.iter_mut().zip(table.iter()) {
            destination.count += source.count;
            destination.total += source.total;
            destination.min = destination.min.min(source.min);
            destination.max = destination.max.max(source.max);
        }
    }

    let mut rows: Vec<(&str, &Slot)> = Vec::with_capacity(CITY_NAMES.len());
    for name in CITY_NAMES {
        let bytes = name.as_bytes();
        let mut word = 0u64;
        for (index, &byte) in bytes[..bytes.len().min(8)].iter().enumerate() {
            word |= (byte as u64) << (index * 8);
        }
        let slot = &merged[hash_word(word)];
        if slot.count > 0 {
            rows.push((name, slot));
        }
    }
    rows.sort_unstable_by_key(|(name, _)| name.as_bytes());

    let mut output = String::with_capacity(32 * 1024);
    for (name, slot) in rows {
        let count = slot.count as i64;
        let _ = writeln!(
            output,
            "{}={};{};{}({}/{})",
            name,
            slot.min,
            slot.max,
            slot.total / count,
            slot.total,
            count
        );
    }
    output
}

fn main() {
    let expect_output = std::fs::read_to_string(common::OUTPUT_PATH).unwrap();
    let timer = Timer::new();
    let got = solution(common::MEASUREMENTS_PATH);
    println!("Elapsed: {}ms", timer.elapsed_as_millis());
    assert_eq!(expect_output, got);
}
