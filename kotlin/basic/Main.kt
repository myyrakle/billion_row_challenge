import java.io.BufferedReader
import java.io.File
import kotlin.system.measureTimeMillis

data class Stats(
    var min: Long,
    var max: Long,
    var sum: Long,
    var count: Long
)

const val OUTPUT_PATH = "outputs.txt"
const val MEASUREMENTS_PATH = "measurements.txt"

fun solution(inputPath: String): String {
    val stationStats = mutableMapOf<String, Stats>()

    // 파일을 한 줄씩 읽어서 메모리 절약
    File(inputPath).bufferedReader().use { reader ->
        reader.forEachLine { line ->
            val parts = line.split(';')
            if (parts.size != 2) return@forEachLine

            val station = parts[0]
            val temp = parts[1].toLong()

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
    val expectOutput = File(OUTPUT_PATH).readText()

    // 타이머 시작 및 솔루션 실행
    var got = ""
    val elapsed = measureTimeMillis {
        got = solution(MEASUREMENTS_PATH)
    }

    println("Elapsed: ${elapsed}ms")

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
