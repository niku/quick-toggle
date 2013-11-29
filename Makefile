CASK ?= cask
export EMACS ?= emacs

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

test: elpa
	${CASK} exec ert-runner

elpa: Cask
	${CASK} install
	find ${PKGDIR}/el-mock* -name *.elc -exec rm {} \; # avoid el-mock bytecompile issue
	touch $@
