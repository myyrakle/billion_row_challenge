;;; Common Lisp Basic Implementation for Billion Row Challenge

(defpackage :billion-row-challenge
  (:use :cl)
  (:export :main))

(in-package :billion-row-challenge)

;;; 파일 경로 상수
(defconstant +output-path+ "outputs.txt")
(defconstant +measurements-path+ "measurements.txt")

;;; 통계 데이터 구조
(defstruct status
  (min most-positive-fixnum :type integer)
  (max most-negative-fixnum :type integer)
  (total 0 :type integer)
  (count 0 :type integer))

;;; 문자열을 구분자로 분리
(defun split-string (string delimiter)
  "문자열을 구분자로 분리하여 리스트 반환"
  (let ((pos (position delimiter string)))
    (if pos
        (list (subseq string 0 pos)
              (subseq string (1+ pos)))
        nil)))

;;; 문자열을 개행 문자로 분리
(defun split-lines (string)
  "문자열을 개행 문자로 분리하여 리스트 반환"
  (let ((lines '())
        (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
          do (progn
               (push (subseq string start i) lines)
               (setf start (1+ i)))
          finally (when (< start (length string))
                    (push (subseq string start) lines)))
    (nreverse lines)))

;;; 솔루션 함수
(defun solution (path)
  "파일을 한 줄씩 읽어서 통계 계산"
  (let ((status-map (make-hash-table :test 'equal)))

    ;; 파일을 한 줄씩 읽기
    (with-open-file (stream path :direction :input)
      (loop for line = (read-line stream nil)
            while line
            do (let ((parts (split-string line #\;)))
                 (when (= (length parts) 2)
                   (let ((city-name (first parts))
                         (measurement-str (second parts)))
                     ;; 숫자로 변환
                     (handler-case
                         (let ((measurement (parse-integer measurement-str)))
                           ;; HashMap에서 도시 찾기
                           (let ((status (gethash city-name status-map)))
                             (if status
                                 ;; 기존 통계 업데이트
                                 (progn
                                   (setf (status-min status)
                                         (min (status-min status) measurement))
                                   (setf (status-max status)
                                         (max (status-max status) measurement))
                                   (setf (status-total status)
                                         (+ (status-total status) measurement))
                                   (setf (status-count status)
                                         (1+ (status-count status))))
                                 ;; 새 도시 추가
                                 (setf (gethash city-name status-map)
                                       (make-status :min measurement
                                                    :max measurement
                                                    :total measurement
                                                    :count 1)))))
                       (parse-error () nil)))))))

    ;; 도시 이름으로 정렬
    (let ((sorted-cities (sort (loop for city being the hash-keys of status-map
                                     collect city)
                               #'string<)))

      ;; 결과 문자열 생성
      (with-output-to-string (result)
        (dolist (city-name sorted-cities)
          (let* ((status (gethash city-name status-map))
                 (avg (floor (status-total status) (status-count status))))
            (format result "~A=~D;~D;~D(~D/~D)~%"
                    city-name
                    (status-min status)
                    (status-max status)
                    avg
                    (status-total status)
                    (status-count status))))))))

;;; 파일 전체 읽기
(defun read-file (path)
  "파일 전체를 문자열로 읽기"
  (with-open-file (stream path :direction :input)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))

;;; 메인 함수
(defun main ()
  "프로그램 진입점"
  ;; 예상 출력 읽기
  (let ((expected-output (read-file +output-path+)))

    ;; 타이머 시작
    (let ((start-time (get-internal-real-time)))
      (let ((result (solution +measurements-path+)))
        (let* ((end-time (get-internal-real-time))
               (elapsed-ms (floor (* (- end-time start-time) 1000)
                                  internal-time-units-per-second)))

          (format t "Elapsed: ~Dms~%" elapsed-ms)

          ;; 결과 비교
          (if (string= expected-output result)
              (format t "Matched!~%")
              (progn
                (format t "Output does not match expected!~%")

                ;; 디버깅용 첫 몇 줄 출력
                (let ((expected-lines (split-lines expected-output))
                      (result-lines (split-lines result)))
                  (format t "Expected first 3 lines:~%")
                  (loop for i from 0 below (min 3 (length expected-lines))
                        do (format t "  ~A~%" (nth i expected-lines)))

                  (format t "Got first 3 lines:~%")
                  (loop for i from 0 below (min 3 (length result-lines))
                        do (format t "  ~A~%" (nth i result-lines)))))))))))
