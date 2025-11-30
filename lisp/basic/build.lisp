;;; Build and Run Script for Common Lisp Implementation

(format t "Loading main.lisp...~%")

;; 현재 디렉토리를 lisp/basic으로 변경
(let ((script-dir (make-pathname :directory (pathname-directory *load-truename*))))
  (load (merge-pathnames "main.lisp" script-dir)))

(format t "Loaded main.lisp successfully~%")

;;; 실행
(format t "Calling main function...~%")
(in-package :billion-row-challenge)
(main)

(format t "Main function completed~%")

;;; 종료
#+sbcl (sb-ext:exit)
#+clisp (ext:exit)
#+ccl (ccl:quit)
#+ecl (ext:quit)
