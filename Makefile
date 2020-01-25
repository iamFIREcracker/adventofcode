.PHONY: pmdb vendor test test-sbcl test-ros

lisps := $(shell find .  -type f \( -iname \*.asd -o -iname \*.lisp \) ! -name make-quickutils.lisp)

cl-print-version-args := --eval '\
	(progn \
		(print (lisp-implementation-version)) \
		(terpri))'

cl-test-args := --eval '\
	(progn \
		(ql:quickload :aoc/tests :verbose T) \
		(let ((exit-code 0)) \
			(handler-case (time (asdf:test-system :aoc)) \
				(error (c) \
					(format T "~&~A~%" c) \
					(setf exit-code 1))) \
			(uiop:quit exit-code)))'

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

test-ccl: $(lisps)
	ccl $(cl-test-args)

test-sbcl: $(lisps)
	sbcl --noinform $(cl-test-args)

test-ros: $(lisps)
	ros run $(cl-print-version-args) $(cl-test-args)
