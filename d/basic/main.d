import std.stdio;
import std.string;
import std.conv;
import std.algorithm;
import std.array;
import std.datetime.stopwatch;
import std.file;

struct Status {
    long min = long.max;
    long max = long.min;
    long total = 0;
    long count = 0;

    void update(long measurement) {
        if (measurement < min) min = measurement;
        if (measurement > max) max = measurement;
        total += measurement;
        count++;
    }
}

string solution(string path) {
    Status[string] map;

    // 파일을 한 줄씩 읽기
    auto file = File(path, "r");
    foreach (line; file.byLine()) {
        // 세미콜론으로 분리
        auto parts = line.split(';');
        if (parts.length != 2) continue;

        string cityName = parts[0].idup; // 문자열 복사 (byLine은 버퍼 재사용)
        long measurement = to!long(parts[1]);

        // HashMap에 업데이트
        if (cityName in map) {
            map[cityName].update(measurement);
        } else {
            Status status;
            status.min = measurement;
            status.max = measurement;
            status.total = measurement;
            status.count = 1;
            map[cityName] = status;
        }
    }
    file.close();

    // 정렬
    auto sortedKeys = map.keys.sort!("a < b").array;

    // 결과 생성
    string result;
    foreach (cityName; sortedKeys) {
        auto status = map[cityName];
        long avg = status.total / status.count;
        result ~= format("%s=%d;%d;%d(%d/%d)\n",
                        cityName,
                        status.min,
                        status.max,
                        avg,
                        status.total,
                        status.count);
    }

    return result;
}

void main() {
    string outputPath = "outputs.txt";
    string measurementsPath = "measurements.txt";

    string expectedOutput = readText(outputPath);

    auto sw = StopWatch(AutoStart.yes);
    string result = solution(measurementsPath);
    sw.stop();

    writefln("Elapsed: %dms", sw.peek.total!"msecs");

    if (expectedOutput == result) {
        writeln("Matched!");
    } else {
        writeln("Output does not match expected!");
        // 디버깅용 첫 몇 줄 출력
        auto expectedLines = expectedOutput.split("\n");
        auto resultLines = result.split("\n");
        writeln("Expected first 3 lines:");
        foreach (i; 0 .. min(3, expectedLines.length)) {
            writeln("  ", expectedLines[i]);
        }
        writeln("Got first 3 lines:");
        foreach (i; 0 .. min(3, resultLines.length)) {
            writeln("  ", resultLines[i]);
        }
    }
}
