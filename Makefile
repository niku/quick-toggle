EMACS=emacs

.PHONY : build test

build :
	$(EMACS) --batch -Q -L . --eval \
		"(progn \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))" *.el

test: build
	$(EMACS) --batch -Q -L . -l test/run-test.el
