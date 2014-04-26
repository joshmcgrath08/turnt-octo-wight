install_dependencies:
	cabal install --only-dependencies

.PHONY: tests
tests: stress_tests instrumented_tests

%_tests:
# expected to fail
	-make -C test/atomicity_violation $*
# not expected to fail
	make -C test/mergesort $*
# expected to fail
	-make -C test/order_violation $*
# not expected to fail
	-make -C test/single_thread $*
