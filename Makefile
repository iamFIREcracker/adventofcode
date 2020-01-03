.PHONY: vendor clean binary-sbcl binary

lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \))

all: test

# Tests -----------------------------------------------------------------------
test-sbcl: $(lisps)
	sbcl --noinform --load "build.lisp" --eval '(1am:run)' --non-interactive

test: test-sbcl
