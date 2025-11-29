// Swift Basic Implementation for Billion Row Challenge
import Foundation

// 통계 데이터 구조
struct Status {
    var min: Int64
    var max: Int64
    var total: Int64
    var count: Int64

    init(measurement: Int64) {
        self.min = measurement
        self.max = measurement
        self.total = measurement
        self.count = 1
    }

    mutating func update(measurement: Int64) {
        if measurement < min {
            min = measurement
        }
        if measurement > max {
            max = measurement
        }
        total += measurement
        count += 1
    }
}

// 파일 경로 상수
let OUTPUT_PATH = "outputs.txt"
let MEASUREMENTS_PATH = "measurements.txt"

// 버퍼를 사용한 라인 단위 파일 읽기
class LineReader {
    let fileHandle: FileHandle
    let delimiter: Data
    let chunkSize: Int
    var buffer: Data
    var isAtEnd: Bool

    init?(path: String, delimiter: String = "\n", chunkSize: Int = 4096) {
        guard let fileHandle = FileHandle(forReadingAtPath: path) else {
            return nil
        }
        self.fileHandle = fileHandle
        self.delimiter = delimiter.data(using: .utf8)!
        self.chunkSize = chunkSize
        self.buffer = Data()
        self.isAtEnd = false
    }

    deinit {
        fileHandle.closeFile()
    }

    func nextLine() -> String? {
        while true {
            // 버퍼에서 개행 문자 찾기
            if let range = buffer.range(of: delimiter) {
                let lineData = buffer.subdata(in: 0..<range.lowerBound)
                buffer.removeSubrange(0..<range.upperBound)

                if let line = String(data: lineData, encoding: .utf8) {
                    return line
                }
            }

            // 버퍼에 개행이 없으면 더 읽기
            if isAtEnd {
                // 파일 끝이고 버퍼에 남은 데이터가 있으면 반환
                if !buffer.isEmpty {
                    defer { buffer.removeAll() }
                    return String(data: buffer, encoding: .utf8)
                }
                return nil
            }

            let chunk = fileHandle.readData(ofLength: chunkSize)
            if chunk.isEmpty {
                isAtEnd = true
            } else {
                buffer.append(chunk)
            }
        }
    }
}

func solution(path: String) -> String {
    var statusMap: [String: Status] = [:]

    // 파일을 한 줄씩 읽기
    guard let reader = LineReader(path: path) else {
        fatalError("Could not open file: \(path)")
    }

    while let line = reader.nextLine() {
        // 세미콜론으로 분리
        let parts = line.split(separator: ";", maxSplits: 1)
        guard parts.count == 2 else {
            continue
        }

        let cityName = String(parts[0])
        guard let measurement = Int64(parts[1]) else {
            continue
        }

        // HashMap 업데이트
        if var status = statusMap[cityName] {
            status.update(measurement: measurement)
            statusMap[cityName] = status
        } else {
            statusMap[cityName] = Status(measurement: measurement)
        }
    }

    // 도시 이름으로 정렬
    let sortedCities = statusMap.keys.sorted()

    // 결과 문자열 생성
    var result = ""
    for cityName in sortedCities {
        let status = statusMap[cityName]!
        let avg = status.total / status.count
        result += "\(cityName)=\(status.min);\(status.max);\(avg)(\(status.total)/\(status.count))\n"
    }

    return result
}

func main() {
    // 예상 출력 읽기
    guard let expectedOutput = try? String(contentsOfFile: OUTPUT_PATH, encoding: .utf8) else {
        fatalError("Could not read output file")
    }

    // 타이머 시작
    let startTime = Date()
    let result = solution(path: MEASUREMENTS_PATH)
    let elapsed = Date().timeIntervalSince(startTime)
    let elapsedMs = Int(elapsed * 1000)

    print("Elapsed: \(elapsedMs)ms")

    // 결과 비교
    if expectedOutput == result {
        print("Matched!")
    } else {
        print("Output does not match expected!")

        // 디버깅용 첫 몇 줄 출력
        let expectedLines = expectedOutput.components(separatedBy: "\n")
        let resultLines = result.components(separatedBy: "\n")

        print("Expected first 3 lines:")
        for i in 0..<min(3, expectedLines.count) {
            print("  \(expectedLines[i])")
        }

        print("Got first 3 lines:")
        for i in 0..<min(3, resultLines.count) {
            print("  \(resultLines[i])")
        }
    }
}

// 메인 실행
main()
