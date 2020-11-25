.PHONY: pmdb vendor test test-sbcl test-ros

lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \) ! -name make-quickutils.lisp)

all: test

# PMDB ------------------------------------------------------------------------
pmdb: pmdb.lisp
pmdb.lisp:
	cp ~/.lisp/pmdb.lisp pmdb.lisp

# Vendor ----------------------------------------------------------------------
vendor: vendor/quickutils.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive

# Tests -----------------------------------------------------------------------
test: test-sbcl

test-sbcl: $(lisps)
	sbcl \
	    --noinform --non-interactive \
	    --load "build/info.lisp" \
	    --load "build/test.lisp"

test-ros: $(lisps)
	ros \
	    --load "build/info.lisp" \
	    --load "build/test.lisp"
