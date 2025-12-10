import kotlin.time.measureTime
import kotlinx.cinterop.*
import platform.posix.*

data class Stats(
    var min: Long,
    var max: Long,
    var sum: Long,
    var count: Long
)

const val OUTPUT_PATH = "outputs.txt"
const val MEASUREMENTS_PATH = "measurements.txt"

@OptIn(ExperimentalForeignApi::class)
fun readFile(path: String): String {
    val file = fopen(path, "r") ?: throw Exception("Cannot open file: $path")
    try {
        val buffer = StringBuilder()
        memScoped {
            val readBuffer = allocArray<ByteVar>(65536)
            while (true) {
                val line = fgets(readBuffer, 65536, file)
                if (line == null) break
                buffer.append(line.toKString())
            }
        }
        return buffer.toString()
    } finally {
        fclose(file)
    }
}

@OptIn(ExperimentalForeignApi::class)
fun solution(inputPath: String): String {
    val stationStats = mutableMapOf<String, Stats>()

    val file = fopen(inputPath, "r") ?: throw Exception("Cannot open file: $inputPath")
    try {
        memScoped {
            val lineBuffer = allocArray<ByteVar>(256)

            while (true) {
                val line = fgets(lineBuffer, 256, file)
                if (line == null) break

                val lineStr = line.toKString().trim()
                if (lineStr.isEmpty()) continue

                val parts = lineStr.split(';')
                if (parts.size != 2) continue

                val station = parts[0]
                val temp = parts[1].toLongOrNull() ?: continue

                val stats = stationStats[station]
                if (stats != null) {
                    stats.min = minOf(stats.min, temp)
                    stats.max = maxOf(stats.max, temp)
                    stats.sum += temp
                    stats.count++
                } else {
                    stationStats[station] = Stats(
                        min = temp,
                        max = temp,
                        sum = temp,
                        count = 1
                    )
                }
            }
        }
    } finally {
        fclose(file)
    }

    // 지역명으로 정렬
    val sortedStations = stationStats.keys.sorted()

    val result = StringBuilder()
    for (station in sortedStations) {
        val stats = stationStats[station]!!
        val mean = stats.sum / stats.count // 정수 나눗셈
        result.append("$station=${stats.min};${stats.max};$mean(${stats.sum}/${stats.count})\n")
    }

    return result.toString()
}

fun main() {
    // 기대 출력 읽기
    val expectOutput = readFile(OUTPUT_PATH)

    // 타이머 시작 및 솔루션 실행
    var got = ""
    val elapsed = measureTime {
        got = solution(MEASUREMENTS_PATH)
    }

    println("Elapsed: ${elapsed.inWholeMilliseconds}ms")

    // 검증
    if (expectOutput == got) {
        println("Matched")
    } else {
        println("Expect:")
        println(expectOutput)
        println("Got:")
        println(got)
        throw Exception("Not matched")
    }
}
