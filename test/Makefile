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

all-tests: test-hello-world test-global-scope test-func1 test-func2 test-func3 test-pointer1 test-while1 test-for1 test-malloc1 test-free1 test-testcase1 test-testcase2 test-testcase3 test-array1 test-lib1 test-gcd1 test-struct-access1 test-bool1 test-bool2 test-arraypt1 test-linkedlist1 test-linkedlist2 test-class1 test-class2 test-class3 test-testcase4 test-struct-malloc1 test-negative1 test-double1 test-double2
	@echo "\033[0;32m========= All Tests Passed! ==========\033[0m"
define test-compile-fail=
@echo "Testing '$1.jt', should fail to compile..."
@../src/jateste.native $1.jt 2>&1 | diff --brief $1.out -
@echo " \033[0;32m ---->  \033[0mTest passed!"
endef

all-tests: runtime-tests compile-tests
	@echo ""
	@echo "\033[0;32m=========== All Tests Passed! ============\033[0m"

runtime-tests: test-hello-world test-global-scope test-func1 test-func2 
	@echo "\033[0;32m========= Runtime Tests Passed! ==========\033[0m"

compile-tests: test-local-var-fail test-no-main-fail return-fail1 struct-access-fail1 invalid-assignment-fail1 class1-var-fail1 class2-method-args-fail
	@echo "\033[0;32m======= Compilation Tests Passed! ========\033[0m"

.PHONY: clean
clean:
	rm -rf *.ll

test-hello-world:
	$(call run-test,hello-world)

test-global-scope:
	$(call run-test,global-scope)

test-func1:
	$(call run-test,test-func1)

test-func2:
	$(call run-test,test-func2)

test-func3:
	$(call run-test,test-func3)


test-pointer1:
	$(call run-test,test-pointer1)

test-while1:
	$(call run-test,test-while1)

test-for1:
	$(call run-test,test-for1)

test-malloc1:
	$(call run-test,test-malloc1)

test-free1:
	$(call run-test,test-free1)

test-testcase1:
	$(call run-test,test-testcase1)

test-testcase2:
	$(call run-test,test-testcase2)

test-testcase3:
	$(call run-test,test-testcase3)

test-testcase4:
	$(call run-test,test-testcase4)

test-struct-malloc1:
	$(call run-test,test-struct-malloc1)

test-gcd1:
	$(call run-test,test-gcd1)

test-array1:
	$(call run-test,test-array1)

test-lib1:
	$(call run-test,test-lib1)

test-struct-access1:
	$(call run-test,test-struct-access1)

test-bool1:
	$(call run-test,test-bool1)

test-bool2:
	$(call run-test,test-bool2)

test-arraypt1:
	$(call run-test,test-arraypt1)

test-negative1:
	$(call run-test,test-negative1)

test-double1:
	$(call run-test,test-double1)

test-double2:
	$(call run-test,test-double2)




# LinkedList library tests
test-linkedlist1:
	$(call run-test,test-linkedlist1)

test-linkedlist2:
	$(call run-test,test-linkedlist2)

# Object-oriented tests
test-class1:
	$(call run-test,test-class1)

test-class2:
	$(call run-test,test-class2)

test-class3:
	$(call run-test,test-class3)








# Tests that should fail

test-local-var-fail:
	$(call test-compile-fail,local-var-fail)

test-no-main-fail:
	$(call test-compile-fail,no-main-fail)

return-fail1:
	$(call test-compile-fail,return-fail1)

struct-access-fail1:
	$(call test-compile-fail,struct-access-fail1)

invalid-assignment-fail1:
	$(call test-compile-fail,invalid-assignment-fail1)

class1-var-fail1:
	$(call test-compile-fail,class1-var-fail1)

class2-method-args-fail:
	$(call test-compile-fail,class2-method-args-fail)





