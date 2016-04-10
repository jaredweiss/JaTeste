#                             JaTeste!                                  #
#                                                                       #
#                    Special Makefile for running tests                 #
#                                                                       #
#########################################################################

define run-test =
@echo "Testing '$1.jt'"
@../src/jateste.native $1.jt > /dev/null
@lli $1.ll | diff --brief $1.out -
@echo " \033[0;32m ---->  \033[0mTest passed!"
endef

all-tests: test-hello-world test-global-scope
	@echo "\033[0;32m========= All Tests Passed! ==========\033[0m"


.PHONY: clean
clean:
	rm -rf *.ll

test-hello-world:
	$(call run-test,hello-world)

test-global-scope:
	$(call run-test,global-scope)
