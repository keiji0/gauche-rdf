TEST_LOG = test.log
FILES = *.scm.test

.PHONY : all clean test

all: ; @echo "make command: check, clean"

clean:
	@rm -f $(TEST_LOG)

test:
	@for i in $(FILES); do sh test $$i >> $(TEST_LOG); done
