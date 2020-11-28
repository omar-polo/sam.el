.POSIX:
EMACS =		emacs

compile: sam.elc sam-test.elc

sam-test.elc: sam.elc

check: sam-test.elc
	${EMACS} -Q --batch -L . -l sam-test.elc -f ert-run-tests-batch

clean:
	rm -f *.elc

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} -Q --batch -L . -f batch-byte-compile $<
