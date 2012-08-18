DIST_DIR=$(PWD)/dist

.PHONY: all
all:
	cabal install --prefix=$(DIST_DIR) --user

.PHONY: test
test:
	runhaskell ./Tests.hs
