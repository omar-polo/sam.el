;;; sam-test.el --- sam test suite.  -*- lexical-binding: t -*-

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

;; (ert-run-tests-interactively t)
