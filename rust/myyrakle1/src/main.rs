#![feature(portable_simd)]

pub mod common;
use common::Timer;
use memmap2::{Advice, Mmap};
use std::fmt::Write;
use std::simd::prelude::*;

const HASH_SIZE: usize = 512;
const HASH_MASK: usize = HASH_SIZE - 1;
const HASH_MULT: u64 = 0xaf7b84ccc0308f9b;

#[inline(always)]
fn hash_word(w: u64) -> usize {
    (w.wrapping_mul(HASH_MULT) >> 54) as usize & HASH_MASK
}

/// 호출자는 `pos + 8 <= data.len()`을 보장해야 한다.
#[inline(always)]
unsafe fn load_u64(data: &[u8], pos: usize) -> u64 {
    debug_assert!(pos + 8 <= data.len());
    u64::from_le((data.as_ptr().add(pos) as *const u64).read_unaligned())
}

#[inline(always)]
fn zero_byte_mask(x: u64) -> u64 {
    x.wrapping_sub(0x0101010101010101) & !x & 0x8080808080808080
}

#[inline(always)]
fn mask_name_word(w: u64, name_len: usize) -> u64 {
    if name_len >= 8 {
        w
    } else {
        w & ((1u64 << (name_len * 8)) - 1)
    }
}

/// 숫자 시작 위치의 8바이트와 자릿수를 받아 값을 돌려준다 (자릿수는 호출자가 이미 알 때).
#[inline(always)]
fn parse_digits_swar(w8: u64, nd: usize) -> i32 {
    debug_assert!(nd >= 1 && nd <= 8);
    let sw = w8.wrapping_sub(0x3030303030303030) << ((8 - nd) * 8);
    let lo = (sw & 0x000F000F000F000F).wrapping_mul(10) + ((sw >> 8) & 0x000F000F000F000F);
    let q = (lo & 0x0000FFFF0000FFFF).wrapping_mul(100) + ((lo >> 16) & 0x0000FFFF0000FFFF);
    let v = (q & 0xFFFF_FFFF).wrapping_mul(10000) + (q >> 32);
    v as i32
}

/// 숫자 시작 위치의 8바이트를 받아 (값, 자릿수)를 돌려준다.
/// 개행이 8바이트 안에 없으면 8자리 숫자로 간주한다(생성기 값 범위 0..50000000, 최대 8자리).
#[inline(always)]
fn parse_number_swar(w8: u64) -> (i32, usize) {
    let nl_mask = zero_byte_mask(w8 ^ 0x0a0a0a0a0a0a0a0a);
    let nd = if nl_mask == 0 {
        8
    } else {
        (nl_mask.trailing_zeros() >> 3) as usize
    };
    (parse_digits_swar(w8, nd), nd)
}

/// 호출자는 `;`가 나올 때까지 8바이트 단위 로드가 범위 안임을 보장해야 한다.
#[inline(always)]
unsafe fn find_semicolon_swar(data: &[u8], start: usize) -> usize {
    const SEMI: u64 = 0x3b3b3b3b3b3b3b3b;
    let mut pos = start;
    loop {
        let m = zero_byte_mask(load_u64(data, pos) ^ SEMI);
        if m != 0 {
            return pos - start + (m.trailing_zeros() >> 3) as usize;
        }
        pos += 8;
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
struct Slot {
    total: i64,
    word: u64,
    name_off: usize,
    count: i32,
    min: i32,
    max: i32,
    name_len: u16,
}

const EMPTY_SLOT: Slot = Slot {
    total: 0,
    word: 0,
    name_off: 0,
    count: 0,
    min: i32::MAX,
    max: -1,
    name_len: 0,
};

/// fast path가 8바이트/32바이트 단위로 앞서 읽어도 mmap 범위를 넘지 않는 상한.
#[inline(always)]
fn fast_limit(len: usize, end: usize) -> usize {
    end.min(len.saturating_sub(64))
}

#[inline(always)]
fn update_slot(
    table: &mut [Slot; HASH_SIZE],
    word: u64,
    name_len: usize,
    name_off: usize,
    value: i32,
) {
    let slot = &mut table[hash_word(word)];
    if slot.count == 0 {
        slot.word = word;
        slot.name_len = name_len as u16;
        slot.name_off = name_off;
    }
    slot.count += 1;
    slot.total += value as i64;
    if value > slot.max {
        slot.max = value;
    }
    if value < slot.min {
        slot.min = value;
    }
}

fn worker_scalar(data: &[u8], start: usize, end: usize, table: &mut [Slot; HASH_SIZE]) {
    let mut p = start;
    while p < end {
        let name_off = p;
        let mut name_len = 0;
        while data[p + name_len] != b';' {
            name_len += 1;
        }
        let mut word = 0u64;
        for i in 0..name_len.min(8) {
            word |= (data[p + i] as u64) << (i * 8);
        }
        p += name_len + 1;

        let mut value = 0i32;
        while data[p] != b'\n' {
            value = value * 10 + (data[p] - b'0') as i32;
            p += 1;
        }
        p += 1;

        update_slot(table, word, name_len, name_off, value);
    }
}

fn worker_swar(data: &[u8], start: usize, end: usize, table: &mut [Slot; HASH_SIZE]) {
    let fast_end = fast_limit(data.len(), end);
    let mut p = start;
    while p < fast_end {
        let name_off = p;
        // 안전성: p < fast_end <= len - 64 이고 이름(<=22B) + ';' + 숫자(<=8B) + '\n'
        // 이 64바이트 안에 있으므로 아래 로드는 전부 범위 안이다.
        let name_len = unsafe { find_semicolon_swar(data, p) };
        let word = mask_name_word(unsafe { load_u64(data, p) }, name_len);
        p += name_len + 1;
        let (value, nd) = parse_number_swar(unsafe { load_u64(data, p) });
        p += nd + 1;
        update_slot(table, word, name_len, name_off, value);
    }
    worker_scalar(data, p, end, table);
}

/// 한 번의 32바이트 SIMD 로드로 `;`와 `\n` 위치를 동시에 찾아
/// (이름 길이, 레코드 전체 길이)를 돌려준다 (x86 AVX2, ARM NEON으로 컴파일됨).
/// 유효 레코드는 최대 32바이트(이름<=22 + ';' + 숫자<=8 + '\n')이므로 둘 다 항상 윈도우 안에 있다.
/// 호출자는 `pos + 32 <= data.len()`을 보장해야 한다.
#[inline(always)]
unsafe fn scan_record_simd(data: &[u8], pos: usize) -> (usize, usize) {
    debug_assert!(pos + 32 <= data.len());
    let v = Simd::<u8, 32>::from_slice(std::slice::from_raw_parts(data.as_ptr().add(pos), 32));
    let semi = v.simd_eq(Simd::splat(b';')).to_bitmask();
    let nl = v.simd_eq(Simd::splat(b'\n')).to_bitmask();
    debug_assert!(semi != 0 && nl != 0, "record markers not in 32B window at pos {pos}");
    (
        semi.trailing_zeros() as usize,
        nl.trailing_zeros() as usize + 1,
    )
}

/// 청크를 3개 레인으로 쪼개 한 루프에서 인터리빙 처리한다.
/// 레인 간 파싱 의존성이 없어 명령어 수준 병렬성(ILP)으로 메모리 지연을 숨긴다.
/// start/end는 레코드 경계여야 하며 청크는 반드시 '\n'으로 끝나야 한다.
fn worker_ilp(data: &[u8], start: usize, end: usize, table: &mut [Slot; HASH_SIZE]) {
    const LANES: usize = 3;
    let total = end - start;
    if total < 4096 {
        worker_swar(data, start, end, table);
        return;
    }

    // 레인 경계를 개행 뒤로 정렬
    let mut bounds = [0usize; LANES + 1];
    bounds[0] = start;
    bounds[LANES] = end;
    for i in 1..LANES {
        let mut b = start + total * i / LANES;
        while data[b] != b'\n' {
            b += 1;
        }
        bounds[i] = b + 1;
    }

    let mut p = [bounds[0], bounds[1], bounds[2]];
    let e = [bounds[1], bounds[2], bounds[3]];
    let f = [
        fast_limit(data.len(), e[0]),
        fast_limit(data.len(), e[1]),
        fast_limit(data.len(), e[2]),
    ];

    while p[0] < f[0] && p[1] < f[1] && p[2] < f[2] {
        // 안전성: 레인마다 p < fast_limit <= len - 64 이고 레코드(이름<=22B + ';' +
        // 숫자<=8B + '\n')가 64바이트 안이므로 32바이트 SIMD 로드와 8바이트 로드 모두 범위 안.
        // 다음 레코드 주소(p[i] + rec_len)가 숫자 파싱과 독립적으로 확정되어 의존성 체인이 짧다.
        for i in 0..LANES {
            let name_off = p[i];
            let (name_len, rec_len) = unsafe { scan_record_simd(data, name_off) };
            let word = mask_name_word(unsafe { load_u64(data, name_off) }, name_len);
            let nd = rec_len - name_len - 2;
            let value = parse_digits_swar(unsafe { load_u64(data, name_off + name_len + 1) }, nd);
            p[i] = name_off + rec_len;
            update_slot(table, word, name_len, name_off, value);
        }
    }

    // 남은 구간은 레인별로 드레인 (fast path 잔여분 + 스칼라 꼬리)
    for i in 0..LANES {
        worker_swar(data, p[i], e[i], table);
    }
}

fn solution(path: &str) -> String {
    let file = std::fs::File::open(path).unwrap();
    // ManuallyDrop: 17GB 매핑의 munmap 비용을 지불하지 않고 프로세스 종료에 맡긴다.
    let mmap = std::mem::ManuallyDrop::new(unsafe { Mmap::map(&file).unwrap() });
    let _ = mmap.advise(Advice::Sequential);
    let _ = mmap.advise(Advice::WillNeed);
    #[cfg(target_os = "linux")]
    let _ = mmap.advise(Advice::HugePage);

    let data: &[u8] = &mmap;
    let len = data.len();
    let nthreads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let chunk = (len / nthreads).max(1);

    let mut tables: Vec<Box<[Slot; HASH_SIZE]>> = Vec::with_capacity(nthreads);
    std::thread::scope(|s| {
        let mut handles = Vec::with_capacity(nthreads);
        let mut start = 0usize;
        for _ in 0..nthreads {
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
            handles.push(s.spawn(move || {
                let mut table = Box::new([EMPTY_SLOT; HASH_SIZE]);
                worker_ilp(data, start, end, &mut table);
                table
            }));
            start = end;
        }
        for h in handles {
            tables.push(h.join().unwrap());
        }
    });

    let mut merged = Box::new([EMPTY_SLOT; HASH_SIZE]);
    for t in &tables {
        for (dst, src) in merged.iter_mut().zip(t.iter()) {
            if src.count == 0 {
                continue;
            }
            if dst.count == 0 {
                dst.word = src.word;
                dst.name_len = src.name_len;
                dst.name_off = src.name_off;
            }
            dst.count += src.count;
            dst.total += src.total;
            if src.max > dst.max {
                dst.max = src.max;
            }
            if src.min < dst.min {
                dst.min = src.min;
            }
        }
    }

    let mut rows: Vec<&Slot> = merged.iter().filter(|s| s.count > 0).collect();
    rows.sort_by_key(|s| &data[s.name_off..s.name_off + s.name_len as usize]);

    let mut out = String::with_capacity(32 * 1024);
    for s in rows {
        let name =
            std::str::from_utf8(&data[s.name_off..s.name_off + s.name_len as usize]).unwrap();
        let avg = s.total / s.count as i64;
        let _ = writeln!(out, "{}={};{};{}({}/{})", name, s.min, s.max, avg, s.total, s.count);
    }
    out
}

fn main() {
    let expect_output = std::fs::read_to_string(common::OUTPUT_PATH).unwrap();

    let timer = Timer::new();
    let got = solution(common::MEASUREMENTS_PATH);
    println!("Elapsed: {}ms", timer.elapsed_as_millis());

    assert_eq!(expect_output, got);
}

#[cfg(test)]
mod tests {
    use super::*;

    // handler/src/main.rs의 CITY_NAMES와 동일한 목록
    const CITY_NAMES: [&str; 100] = [
        "Adenarith", "Amsterdam", "Anápolis", "Aparecida de Goiânia", "Athens",
        "Austin", "Bahrain", "Bangalore", "Barcelona", "Belo Horizonte",
        "Belém", "Boa Vista", "Boston", "Brasília", "Brussels",
        "Bucharest", "Campinas", "Canada", "Central", "Chennai",
        "Chongqing", "Copenhagen", "Cuiabá", "Curitiba", "Dallas",
        "Dublin", "Duque de Caxias", "Feira de Santana", "Fortaleza", "Frankfurt",
        "Gaaphis", "Goiania", "Guadalajara", "Guarulhos", "Helsinki",
        "Hong Kong", "Hyderabad", "Indianapolis", "Ireland", "Istanbul",
        "Juiz de Fora", "Kiev", "Kolkata", "Krofast", "Krore",
        "Larfast", "London", "Londrina", "Los Angeles", "Macapá",
        "Madrid", "Manaus", "Mexico City", "Miami", "Milan",
        "Montreal", "Moscow", "Mumbai", "N. California", "N. Virginia",
        "New Delhi", "New York", "Niterói", "Nova Iguaçu", "Ohio",
        "Oregon", "Osaka", "Osasco", "Oslo", "Palmas",
        "Paris", "Porto Alegre", "Porto Velho", "Prico", "Prover",
        "Pune", "Qreigh", "Qrokwood", "Recife", "Ribeirão Preto",
        "Rio de Janeiro", "Salvador", "Santo André", "Sao Paulo", "Seoul",
        "Singapore", "St. Petersburg", "Stockholm", "Sydney", "São Bernardo do Campo",
        "São Gonçalo", "São José dos Campos", "São Paulo", "Tokyo", "Toronto",
        "Urgtin", "Vancouver", "Vienna", "Warsaw", "Zurich",
    ];

    fn name_word(name: &[u8]) -> u64 {
        let mut w = 0u64;
        for (i, &b) in name.iter().take(8).enumerate() {
            w |= (b as u64) << (i * 8);
        }
        w
    }

    #[test]
    fn all_city_names_hash_to_distinct_slots() {
        let mut seen = std::collections::HashMap::new();
        for name in CITY_NAMES {
            let slot = hash_word(name_word(name.as_bytes()));
            if let Some(prev) = seen.insert(slot, name) {
                panic!("hash collision: {prev} vs {name} -> slot {slot}");
            }
        }
    }

    #[test]
    fn parse_number_swar_parses_digits_before_newline() {
        let cases: &[(&[u8; 8], i32, usize)] = &[
            (b"0\nabcdef", 0, 1),
            (b"123\nabcd", 123, 3),
            (b"9999999\n", 9_999_999, 7),
            (b"10000000", 10_000_000, 8), // 8자리: 개행이 9번째 바이트에 있는 경우
            (b"4567\nxyz", 4567, 4),
        ];
        for (bytes, want, want_nd) in cases {
            let w8 = u64::from_le_bytes(**bytes);
            assert_eq!(parse_number_swar(w8), (*want, *want_nd), "input: {bytes:?}");
        }
    }

    #[test]
    fn find_semicolon_swar_finds_length() {
        let buf = b"Kiev;123\n_____padding________";
        assert_eq!(unsafe { find_semicolon_swar(buf, 0) }, 4);
        let buf2 = "São Bernardo do Campo;42\n________padding________".as_bytes();
        assert_eq!(unsafe { find_semicolon_swar(buf2, 0) }, 22);
        assert_eq!(unsafe { find_semicolon_swar(b"Seoul;100\n_______", 0) }, 5);
    }

    #[test]
    fn mask_name_word_keeps_prefix() {
        assert_eq!(
            mask_name_word(u64::from_le_bytes(*b"Kiev;123"), 4),
            u64::from_le_bytes(*b"Kiev\0\0\0\0")
        );
        assert_eq!(
            mask_name_word(u64::from_le_bytes(*b"Amsterda"), 9),
            u64::from_le_bytes(*b"Amsterda")
        );
    }

    #[test]
    fn worker_swar_aggregates_records() {
        // fast path와 스칼라 폴백(마지막 64바이트 이내) 모두 지나가도록 반복
        let mut input = String::new();
        for _ in 0..20 {
            input.push_str("Seoul;100\n");
            input.push_str("São Paulo;9999999\n");
            input.push_str("Sao Paulo;3\n");
            input.push_str("Ohio;0\n");
        }
        let data = input.as_bytes();

        let mut table = Box::new([EMPTY_SLOT; HASH_SIZE]);
        worker_swar(data, 0, data.len(), &mut table);

        let seoul = &table[hash_word(name_word(b"Seoul"))];
        assert_eq!(
            (seoul.count, seoul.min, seoul.max, seoul.total),
            (20, 100, 100, 2000)
        );
        assert_eq!(seoul.name_len, 5);
        assert_eq!(&data[seoul.name_off..seoul.name_off + 5], b"Seoul");

        let sao = &table[hash_word(name_word("São Paulo".as_bytes()))];
        assert_eq!(
            (sao.count, sao.min, sao.max, sao.total),
            (20, 9_999_999, 9_999_999, 20 * 9_999_999i64)
        );

        let sao_ascii = &table[hash_word(name_word(b"Sao Paulo"))];
        assert_eq!((sao_ascii.count, sao_ascii.min, sao_ascii.max), (20, 3, 3));

        let ohio = &table[hash_word(name_word(b"Ohio"))];
        assert_eq!((ohio.count, ohio.min, ohio.max, ohio.total), (20, 0, 0, 0));

        // 전체 레코드 수 보존 확인
        let total_count: i32 = table.iter().map(|s| s.count).sum();
        assert_eq!(total_count, 80);
    }

    #[test]
    fn worker_scalar_handles_whole_chunk() {
        let input = "Kiev;42\nKiev;7\nTokyo;10000000\n";
        let data = input.as_bytes();
        let mut table = Box::new([EMPTY_SLOT; HASH_SIZE]);
        worker_scalar(data, 0, data.len(), &mut table);

        let kiev = &table[hash_word(name_word(b"Kiev"))];
        assert_eq!((kiev.count, kiev.min, kiev.max, kiev.total), (2, 7, 42, 49));

        let tokyo = &table[hash_word(name_word(b"Tokyo"))];
        assert_eq!(
            (tokyo.count, tokyo.min, tokyo.max, tokyo.total),
            (1, 10_000_000, 10_000_000, 10_000_000)
        );
    }

    #[test]
    fn scan_record_simd_finds_both_markers() {
        for name in CITY_NAMES {
            // 레코드: "<이름>;405\n" + 패딩
            let mut buf = Vec::from(name.as_bytes());
            buf.push(b';');
            buf.extend_from_slice(b"405\n");
            buf.extend_from_slice(&[b'_'; 40]);

            let (name_len, rec_len) = unsafe { scan_record_simd(&buf, 0) };
            assert_eq!(name_len, unsafe { find_semicolon_swar(&buf, 0) }, "{name}");
            assert_eq!(name_len, name.len(), "{name}");
            assert_eq!(rec_len, name.len() + 1 + 3 + 1, "{name}");

            // 비정렬 시작 위치(pos != 0)에서도 동일해야 한다
            let mut buf2 = vec![b'x'; 3];
            buf2.extend_from_slice(&buf);
            let (name_len2, rec_len2) = unsafe { scan_record_simd(&buf2, 3) };
            assert_eq!((name_len2, rec_len2), (name_len, rec_len), "{name} at offset 3");
        }
    }

    #[test]
    fn worker_ilp_matches_scalar() {
        // 결정적 의사난수로 5000 레코드 생성 — 3레인 분할 fast path와 레인별 드레인 경로 모두 커버
        let mut input = String::new();
        let mut state = 0x243F_6A88_85A3_08D3u64;
        for _ in 0..5000 {
            state = state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            let city = CITY_NAMES[(state >> 33) as usize % 100];
            let value = (state >> 3) % 50_000_000;
            input.push_str(&format!("{city};{value}\n"));
        }
        let data = input.as_bytes();

        let mut expect = Box::new([EMPTY_SLOT; HASH_SIZE]);
        worker_scalar(data, 0, data.len(), &mut expect);

        let mut got = Box::new([EMPTY_SLOT; HASH_SIZE]);
        worker_ilp(data, 0, data.len(), &mut got);

        // name_off는 레인 순서에 따라 "같은 이름의 다른 출현 위치"를 가리킬 수 있으므로
        // 집계값과 이름 바이트가 같은지를 비교한다.
        for (i, (e, g)) in expect.iter().zip(got.iter()).enumerate() {
            assert_eq!(
                (e.count, e.min, e.max, e.total, e.word, e.name_len),
                (g.count, g.min, g.max, g.total, g.word, g.name_len),
                "slot {i}"
            );
            assert_eq!(
                &data[e.name_off..e.name_off + e.name_len as usize],
                &data[g.name_off..g.name_off + g.name_len as usize],
                "slot {i} name bytes"
            );
        }
    }

    #[test]
    fn worker_ilp_small_buffer_delegates() {
        // 4096바이트 미만이면 통째로 worker_swar로 위임되는 경로
        let input = "Kiev;42\nKiev;7\nTokyo;10000000\n";
        let data = input.as_bytes();
        let mut table = Box::new([EMPTY_SLOT; HASH_SIZE]);
        worker_ilp(data, 0, data.len(), &mut table);

        let kiev = &table[hash_word(name_word(b"Kiev"))];
        assert_eq!((kiev.count, kiev.min, kiev.max, kiev.total), (2, 7, 42, 49));
    }

    #[test]
    fn solution_end_to_end_small_file() {
        let dir = std::env::temp_dir().join("myyrakle1_e2e_test");
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("measurements_small.txt");

        let mut input = String::new();
        for _ in 0..1000 {
            input.push_str("Seoul;100\nOhio;5\nSão Paulo;7\nSeoul;200\n");
        }
        std::fs::write(&path, &input).unwrap();

        let got = solution(path.to_str().unwrap());
        assert_eq!(
            got,
            "Ohio=5;5;5(5000/1000)\nSeoul=100;200;150(300000/2000)\nSão Paulo=7;7;7(7000/1000)\n"
        );
    }
}
