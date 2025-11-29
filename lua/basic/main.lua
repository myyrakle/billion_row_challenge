-- 파일 경로 상수
local OUTPUT_PATH = "outputs.txt"
local MEASUREMENTS_PATH = "measurements.txt"

-- 문자열 split 함수
local function split(str, delimiter)
    local result = {}
    local pattern = string.format("([^%s]+)", delimiter)
    for token in string.gmatch(str, pattern) do
        table.insert(result, token)
    end
    return result
end

-- 테이블 키들을 정렬된 배열로 반환
local function get_sorted_keys(tbl)
    local keys = {}
    for key in pairs(tbl) do
        table.insert(keys, key)
    end
    table.sort(keys)
    return keys
end

-- 파일을 한 줄씩 읽어서 처리
local function solution(path)
    local status_map = {}

    -- 파일을 한 줄씩 읽기 (메모리 효율적)
    local file = io.open(path, "r")
    if not file then
        error("Could not open file: " .. path)
    end

    for line in file:lines() do
        -- 세미콜론으로 분리
        local parts = split(line, ";")
        if #parts == 2 then
            local city_name = parts[1]
            local measurement = tonumber(parts[2])

            if measurement then
                -- 통계 업데이트
                if status_map[city_name] then
                    local status = status_map[city_name]
                    if measurement < status.min then
                        status.min = measurement
                    end
                    if measurement > status.max then
                        status.max = measurement
                    end
                    status.total = status.total + measurement
                    status.count = status.count + 1
                else
                    -- 새 도시 추가
                    status_map[city_name] = {
                        min = measurement,
                        max = measurement,
                        total = measurement,
                        count = 1
                    }
                end
            end
        end
    end

    file:close()

    -- 도시 이름으로 정렬
    local sorted_cities = get_sorted_keys(status_map)

    -- 결과 문자열 생성
    local result = {}
    for _, city_name in ipairs(sorted_cities) do
        local status = status_map[city_name]
        local avg = math.floor(status.total / status.count)
        local line = string.format("%s=%d;%d;%d(%d/%d)",
            city_name,
            status.min,
            status.max,
            avg,
            status.total,
            status.count)
        table.insert(result, line)
    end

    return table.concat(result, "\n") .. "\n"
end

-- 파일 전체 읽기
local function read_file(path)
    local file = io.open(path, "r")
    if not file then
        error("Could not open file: " .. path)
    end
    local content = file:read("*all")
    file:close()
    return content
end

-- 메인 함수
local function main()
    -- 예상 출력 읽기
    local expected_output = read_file(OUTPUT_PATH)

    -- 타이머 시작
    local start_time = os.clock()
    local result = solution(MEASUREMENTS_PATH)
    local elapsed = os.clock() - start_time
    local elapsed_ms = math.floor(elapsed * 1000)

    print(string.format("Elapsed: %dms", elapsed_ms))

    -- 결과 비교
    if expected_output == result then
        print("Matched!")
    else
        print("Output does not match expected!")

        -- 디버깅용 첫 몇 줄 출력
        local expected_lines = split(expected_output, "\n")
        local result_lines = split(result, "\n")

        print("Expected first 3 lines:")
        for i = 1, math.min(3, #expected_lines) do
            print("  " .. expected_lines[i])
        end

        print("Got first 3 lines:")
        for i = 1, math.min(3, #result_lines) do
            print("  " .. result_lines[i])
        end
    end
end

-- 스크립트 실행
main()
