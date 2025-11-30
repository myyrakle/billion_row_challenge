;;; Scheme Basic Implementation for Billion Row Challenge
;;; GNU Guile 사용

(use-modules (ice-9 rdelim)
             (ice-9 format)
             (srfi srfi-1)
             (srfi srfi-69))

;;; 파일 경로 상수
(define output-path "outputs.txt")
(define measurements-path "measurements.txt")

;;; 문자열을 구분자로 분리
(define (split-string str delimiter)
  "문자열을 구분자로 분리하여 리스트 반환"
  (let ((pos (string-index str delimiter)))
    (if pos
        (list (substring str 0 pos)
              (substring str (+ pos 1) (string-length str)))
        '())))

;;; 솔루션 함수
(define (solution path)
  "파일을 한 줄씩 읽어서 통계 계산"
  (let ((status-map (make-hash-table)))

    ;; 파일을 한 줄씩 읽기
    (with-input-from-file path
      (lambda ()
        (let loop ((line (read-line)))
          (unless (eof-object? line)
            ;; 세미콜론으로 분리
            (let ((parts (split-string line #\;)))
              (when (= (length parts) 2)
                (let ((city-name (car parts))
                      (measurement-str (cadr parts)))
                  ;; 숫자로 변환
                  (catch #t
                    (lambda ()
                      (let ((measurement (string->number measurement-str)))
                        (when measurement
                          ;; HashMap에서 도시 찾기
                          (let ((status (hash-table-ref/default status-map city-name #f)))
                            (if status
                                ;; 기존 통계 업데이트
                                (begin
                                  (vector-set! status 0 (min (vector-ref status 0) measurement))
                                  (vector-set! status 1 (max (vector-ref status 1) measurement))
                                  (vector-set! status 2 (+ (vector-ref status 2) measurement))
                                  (vector-set! status 3 (+ (vector-ref status 3) 1)))
                                ;; 새 도시 추가 (min, max, total, count)
                                (hash-table-set! status-map city-name
                                                (vector measurement measurement measurement 1)))))))
                    (lambda (key . args)
                      #f)))))
            (loop (read-line))))))

    ;; 도시 이름으로 정렬
    (let ((sorted-cities (sort (hash-table-keys status-map) string<?)))

      ;; 결과 문자열 생성
      (with-output-to-string
        (lambda ()
          (for-each
           (lambda (city-name)
             (let* ((status (hash-table-ref status-map city-name))
                    (min-val (vector-ref status 0))
                    (max-val (vector-ref status 1))
                    (total (vector-ref status 2))
                    (count (vector-ref status 3))
                    (avg (quotient total count)))
               (format #t "~a=~a;~a;~a(~a/~a)~%"
                       city-name min-val max-val avg total count)))
           sorted-cities))))))

;;; 파일 전체 읽기
(define (read-file path)
  "파일 전체를 문자열로 읽기"
  (with-input-from-file path
    (lambda ()
      (let ((contents (read-string)))
        (if (eof-object? contents)
            ""
            contents)))))

;;; 문자열을 개행으로 분리
(define (split-lines str)
  "문자열을 개행 문자로 분리"
  (string-split str #\newline))

;;; 메인 함수
(define (main)
  "프로그램 진입점"
  ;; 예상 출력 읽기
  (let ((expected-output (read-file output-path)))

    ;; 타이머 시작
    (let ((start-time (get-internal-real-time)))
      (let ((result (solution measurements-path)))
        (let* ((end-time (get-internal-real-time))
               (elapsed-ms (quotient (* (- end-time start-time) 1000)
                                    internal-time-units-per-second)))

          (format #t "Elapsed: ~ams~%" elapsed-ms)

          ;; 결과 비교
          (if (string=? expected-output result)
              (format #t "Matched!~%")
              (begin
                (format #t "Output does not match expected!~%")

                ;; 디버깅용 첫 몇 줄 출력
                (let ((expected-lines (split-lines expected-output))
                      (result-lines (split-lines result)))
                  (format #t "Expected first 3 lines:~%")
                  (for-each
                   (lambda (line)
                     (format #t "  ~a~%" line))
                   (take expected-lines (min 3 (length expected-lines))))

                  (format #t "Got first 3 lines:~%")
                  (for-each
                   (lambda (line)
                     (format #t "  ~a~%" line))
                   (take result-lines (min 3 (length result-lines))))))))))))

;;; 스크립트 실행
(main)
