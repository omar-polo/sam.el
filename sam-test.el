;;; sam-test.el --- sam test suite.  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(require 'ert)
(require 'sam)

(ert-deftest sam-parse-command-test ()
  (dolist (spec '(("3" "p"  <- "3p")
                  ("3" "p"  <- "3 p")
                  ("34" "p" <- "34\tp")
                  ("" "b"   <- "b")
                  ("32" ""  <- "32")))
    (cl-destructuring-bind (exp-addr exp-cmd _ cmd) spec
      (cl-destructuring-bind (address . command) (sam-parse-command cmd)
        (should (string-equal exp-addr address))
        (should (string-equal exp-cmd  command))))))

;; (ert-run-tests-interactively t)
