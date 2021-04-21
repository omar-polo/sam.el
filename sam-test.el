;;; sam-test.el --- sam test suite.  -*- lexical-binding: t -*-

;;; Commentary:
;; Sam.el test case.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'sam)

(ert-deftest sam-parse-line-test ()
  (dolist (spec '(("3p"    ->  "3" "p")
                  ("3 p"   ->  "3" "p")
                  ("34\tp" -> "34" "p")
                  ("b"     ->   "" "b")
                  ("32"    -> "32" "")))
    (cl-destructuring-bind (cmd _ exp-addr exp-cmd) spec
      (cl-destructuring-bind (address . command) (sam-parse-line cmd)
        (should (string-equal exp-addr address))
        (should (string-equal exp-cmd  command))))))

(ert-deftest sam-parse-command-test ()
  (let ((spec '(("=#" -> "=#" sam-cmd-charoffset))))
    (cl-loop
     for (input _ exp-cmd exp-fn) in spec
     do (cl-destructuring-bind (cmd . fn) (sam-parse-command input)
          (should (string-equal exp-cmd cmd))
          (should (string-equal exp-fn  fn))))))

(ert-deftest sam-parse-delimited-test ()
  (cl-flet ((parse-delimited (in exp)
                             (equal (sam-parse-delimited in)
                                    exp)))
    (cl-loop with spec = '(("/foo/bar" . ("foo" "bar"))
                           ("?foo?bar" . ("foo" "bar"))
                           (",foo,"    . ("foo" ""))
                           ("||"       . ("" ""))
                           ("*foo*bar" . ("foo" "bar"))
                           (".foo.bar" . ("foo" "bar"))
                           ;(".foo...." . ("foo" "..."))
                           )
             for (input . exp) in spec
             do (should (parse-delimited input exp)))))

;; (ert-run-tests-interactively t)

(provide 'sam-test)
;;; sam-test.el ends here
