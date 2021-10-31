lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \) ! -name make-quickutils.lisp)

all: test

# Vendor ----------------------------------------------------------------------
.PHONY: vendor
vendor: vendor/pmdb.lisp vendor/quickutils.lisp
vendor/pmdb.lisp:
	cp ~/.lisp/pmdb.lisp vendor/pmdb.lisp
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive

# Info ------------------------------------------------------------------------
.PHONY: lisp-info
lisp-info:
	sbcl --noinform --quit \
		--load "build/info.lisp"

.PHONY: lisp-info-ros
lisp-info-ros:
	ros \
		--load "build/info.lisp" \

# Tests -----------------------------------------------------------------------
.PHONY: test
test: test-sbcl

.PHONY: test-sbcl
test-sbcl: lisp-info $(lisps)
	sbcl --noinform \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp" \
		--load "build/test.lisp"

.PHONY: test-ros
test-ros: lisp-info-ros $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp" \
		--load "build/test.lisp"
	
