CABALVER := $(shell cabal --numeric-version)
SANDBOX := true
ifneq ($(CABALVER), 1.16.0.2)
  SANDBOX := cabal sandbox init
  CABALJOBS := --jobs
endif

cabal-all : cabal-init
	cabal build $(CABALJOBS)
	cabal copy

cabal-test : cabal-init cabal-test-init cabal-all
	cabal test $(CABALJOBS)
	cabal check

cabal-clean :
	cabal clean

cabal-init :
	$(SANDBOX)
	cabal update
	cabal install $(CABALJOBS) --only-dependencies --enable-tests --enable-benchmarks --force-reinstalls --reorder-goals --max-backjumps=-1
	cabal configure

cabal-test-init:
	cabal configure --enable-tests --enable-benchmarks

.PHONY : cabal-all cabal-clean cabal-init cabal-test cabal-test-init
