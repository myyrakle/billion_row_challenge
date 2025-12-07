import 'dart:io';
import 'dart:convert';

const String outputPath = 'outputs.txt';
const String measurementsPath = 'measurements.txt';

class Stats {
  int min;
  int max;
  int sum;
  int count;

  Stats({
    required this.min,
    required this.max,
    required this.sum,
    required this.count,
  });
}

String solution(String inputPath) {
  final stationStats = <String, Stats>{};

  // 파일을 한 줄씩 읽어서 메모리를 절약
  final file = File(inputPath);

  // RandomAccessFile을 사용하여 바이트 레벨 버퍼링
  final input = file.openSync(mode: FileMode.read);
  final readBuffer = List<int>.filled(1024 * 1024, 0); // 1MB 버퍼
  final lineBuffer = <int>[];

  try {
    while (true) {
      final bytesRead = input.readIntoSync(readBuffer);
      if (bytesRead == 0) break;

      // 읽은 바이트를 한 줄씩 처리
      for (var i = 0; i < bytesRead; i++) {
        final byte = readBuffer[i];

        if (byte == 10) { // '\n'
          // 한 줄 완성 - 처리
          if (lineBuffer.isNotEmpty) {
            final line = utf8.decode(lineBuffer);
            lineBuffer.clear();

            final parts = line.split(';');
            if (parts.length == 2) {
              final station = parts[0];
              final temp = int.parse(parts[1]);

              if (stationStats.containsKey(station)) {
                final s = stationStats[station]!;
                s.min = s.min < temp ? s.min : temp;
                s.max = s.max > temp ? s.max : temp;
                s.sum += temp;
                s.count++;
              } else {
                stationStats[station] = Stats(
                  min: temp,
                  max: temp,
                  sum: temp,
                  count: 1,
                );
              }
            }
          }
        } else {
          lineBuffer.add(byte);
        }
      }
    }

    // 버퍼에 남은 마지막 라인 처리 (개행 없이 끝난 경우)
    if (lineBuffer.isNotEmpty) {
      final line = utf8.decode(lineBuffer);
      final parts = line.split(';');
      if (parts.length == 2) {
        final station = parts[0];
        final temp = int.parse(parts[1]);

        if (stationStats.containsKey(station)) {
          final s = stationStats[station]!;
          s.min = s.min < temp ? s.min : temp;
          s.max = s.max > temp ? s.max : temp;
          s.sum += temp;
          s.count++;
        } else {
          stationStats[station] = Stats(
            min: temp,
            max: temp,
            sum: temp,
            count: 1,
          );
        }
      }
    }
  } finally {
    input.closeSync();
  }

  // 지역명으로 정렬
  final sortedStations = stationStats.keys.toList()..sort();

  final result = StringBuffer();
  for (final station in sortedStations) {
    final s = stationStats[station]!;
    final mean = s.sum ~/ s.count; // 정수 나눗셈
    result.writeln('$station=${s.min};${s.max};$mean(${s.sum}/${s.count})');
  }

  return result.toString();
}

void main() {
  // 기대 출력 읽기
  final expectOutput = File(outputPath).readAsStringSync();

  // 타이머 시작
  final stopwatch = Stopwatch()..start();

  // 솔루션 실행
  final got = solution(measurementsPath);

  // 타이머 종료
  stopwatch.stop();

  print('Elapsed: ${stopwatch.elapsedMilliseconds}ms');

  // 검증
  if (expectOutput == got) {
    print('Matched');
  } else {
    print('Expect:');
    print(expectOutput);
    print('Got:');
    print(got);
    throw Exception('Not matched');
  }
}
