# Julia Basic Implementation for Billion Row Challenge

using Printf

# 통계 데이터 구조
mutable struct Status
    min::Int64
    max::Int64
    total::Int64
    count::Int64
end

# 파일 경로 상수
const OUTPUT_PATH = "outputs.txt"
const MEASUREMENTS_PATH = "measurements.txt"

function solution(path::String)::String
    # Dictionary로 도시별 통계 관리
    status_map = Dict{String, Status}()

    # 파일을 한 줄씩 읽기 (메모리 효율적)
    open(path, "r") do file
        for line in eachline(file)
            # 세미콜론으로 분리
            parts = split(line, ';', limit=2)
            if length(parts) != 2
                continue
            end

            city_name = parts[1]
            measurement = parse(Int64, parts[2])

            # HashMap 업데이트
            if haskey(status_map, city_name)
                status = status_map[city_name]
                status.min = min(status.min, measurement)
                status.max = max(status.max, measurement)
                status.total += measurement
                status.count += 1
            else
                status_map[city_name] = Status(
                    measurement,  # min
                    measurement,  # max
                    measurement,  # total
                    1             # count
                )
            end
        end
    end

    # 도시 이름으로 정렬
    sorted_cities = sort(collect(keys(status_map)))

    # 결과 문자열 생성
    result = IOBuffer()
    for city_name in sorted_cities
        status = status_map[city_name]
        avg = div(status.total, status.count)  # integer division
        @printf(result, "%s=%d;%d;%d(%d/%d)\n",
                city_name,
                status.min,
                status.max,
                avg,
                status.total,
                status.count)
    end

    return String(take!(result))
end

function main()
    # 예상 출력 읽기
    expected_output = read(OUTPUT_PATH, String)

    # 타이머 시작
    start_time = time_ns()
    result = solution(MEASUREMENTS_PATH)
    elapsed_ns = time_ns() - start_time
    elapsed_ms = div(elapsed_ns, 1_000_000)

    println("Elapsed: $(elapsed_ms)ms")

    # 결과 비교
    if expected_output == result
        println("Matched!")
    else
        println("Output does not match expected!")

        # 디버깅용 첫 몇 줄 출력
        expected_lines = split(expected_output, '\n')
        result_lines = split(result, '\n')
        println("Expected first 3 lines:")
        for i in 1:min(3, length(expected_lines))
            println("  ", expected_lines[i])
        end
        println("Got first 3 lines:")
        for i in 1:min(3, length(result_lines))
            println("  ", result_lines[i])
        end
    end
end

# 스크립트로 실행될 때만 main 호출
if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
