# R Basic Implementation for Billion Row Challenge

# 파일 경로 상수
OUTPUT_PATH <- "outputs.txt"
MEASUREMENTS_PATH <- "measurements.txt"

# 솔루션 함수
solution <- function(path) {
  # 환경(해시맵 역할)으로 도시별 통계 관리
  status_map <- new.env(hash = TRUE)

  # 파일을 한 줄씩 읽기 (메모리 효율적)
  con <- file(path, "r")

  tryCatch({
    while (TRUE) {
      line <- readLines(con, n = 1, warn = FALSE)

      # EOF 체크
      if (length(line) == 0) {
        break
      }

      # 세미콜론으로 분리
      parts <- strsplit(line, ";", fixed = TRUE)[[1]]

      if (length(parts) == 2) {
        city_name <- parts[1]
        measurement <- suppressWarnings(as.numeric(parts[2]))

        # 숫자 변환 확인
        if (!is.na(measurement)) {
          # 환경에서 도시 찾기
          if (exists(city_name, envir = status_map)) {
            status <- get(city_name, envir = status_map)
            status$min <- min(status$min, measurement)
            status$max <- max(status$max, measurement)
            status$total <- status$total + measurement
            status$count <- status$count + 1
            assign(city_name, status, envir = status_map)
          } else {
            # 새 도시 추가
            status <- list(
              min = measurement,
              max = measurement,
              total = measurement,
              count = 1
            )
            assign(city_name, status, envir = status_map)
          }
        }
      }
    }
  }, finally = {
    close(con)
  })

  # 도시 이름으로 정렬
  sorted_cities <- sort(ls(status_map))

  # 결과 문자열 생성
  result <- character()

  for (city_name in sorted_cities) {
    status <- get(city_name, envir = status_map)
    avg <- floor(status$total / status$count)

    line_str <- sprintf("%s=%d;%d;%d(%d/%d)",
                       city_name,
                       status$min,
                       status$max,
                       avg,
                       status$total,
                       status$count)
    result <- c(result, line_str)
  }

  # 개행 문자로 연결하고 마지막에 개행 추가
  paste0(paste(result, collapse = "\n"), "\n")
}

# 파일 전체 읽기
read_file <- function(path) {
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

# 메인 함수
main <- function() {
  # 예상 출력 읽기
  expected_output <- read_file(OUTPUT_PATH)

  # 타이머 시작
  start_time <- Sys.time()
  result <- solution(MEASUREMENTS_PATH)
  end_time <- Sys.time()

  elapsed_ms <- floor(difftime(end_time, start_time, units = "secs") * 1000)

  cat(sprintf("Elapsed: %.0fms\n", elapsed_ms))

  # 결과 비교
  if (expected_output == result) {
    cat("Matched!\n")
  } else {
    cat("Output does not match expected!\n")

    # 디버깅용 첫 몇 줄 출력
    expected_lines <- strsplit(expected_output, "\n", fixed = TRUE)[[1]]
    result_lines <- strsplit(result, "\n", fixed = TRUE)[[1]]

    cat("Expected first 3 lines:\n")
    for (i in 1:min(3, length(expected_lines))) {
      cat(sprintf("  %s\n", expected_lines[i]))
    }

    cat("Got first 3 lines:\n")
    for (i in 1:min(3, length(result_lines))) {
      cat(sprintf("  %s\n", result_lines[i]))
    }
  }
}

# 스크립트 실행
main()
