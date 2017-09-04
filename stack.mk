stack-all :
	stack build

stack-test :
	stack --no-terminal --install-ghc ${ARGS} test --bench --only-dependencies
	echo "" | stack --no-terminal ${ARGS} test ":test" --bench --no-run-benchmarks --haddock --no-haddock-deps --coverage

stack-clean :
	stack clean

.PHONY : stack-all stack-clean stack-test
