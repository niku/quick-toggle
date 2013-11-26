CASK ?= cask
export EMACS ?= emacs

test: elpa
	${CASK} exec ert-runner

elpa: Cask
	${CASK} install
