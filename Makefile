all : cabal-all

clean : cabal-clean stack-clean

test : cabal-test stack-test

.PHONY : all clean test

include stack.mk
include cabal.mk
