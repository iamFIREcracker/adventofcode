lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \) ! -name make-quickutils.lisp)

all: test

# Vendor ----------------------------------------------------------------------
.PHONY: vendor
vendor: vendor/pmdb.lisp vendor/quickutils.lisp
vendor/pmdb.lisp:
	cp ~/.lisp/pmdb.lisp vendor/pmdb.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive

# Tests -----------------------------------------------------------------------
.PHONY: test
test: test-sbcl

.PHONY: test-sbcl
test-sbcl: $(lisps)
	sbcl \
	    --noinform --non-interactive \
	    --load "build/info.lisp" \
	    --load "build/test.lisp"

.PHONY: test-ros
test-ros: $(lisps)
	ros \
	    --load "build/info.lisp" \
	    --load "build/test.lisp"
