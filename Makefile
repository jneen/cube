DIST_DIR=$(PWD)/dist

CABAL_OPTS = --prefix=$(DIST_DIR)
CABAL_OPTS += --user
CABAL_OPTS += --enable-documentation

.PHONY: all
all:
	cabal install $(CABAL_OPTS)

.PHONY: test
test:
	runhaskell ./Tests.hs

console:
	GHC_PACKAGE_PATH=./dist/package.conf.inplace: ghci
