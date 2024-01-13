lisps := $(shell find . -follow -type f \( -iname \*.asd -o -iname \*.lisp \) ! -path "./vendor/*")

all: vendor

# Vendor ----------------------------------------------------------------------
.PHONY: vendor
vendor: vendor/pmdb.lisp vendor/quickutils.lisp
vendor/pmdb.lisp:
	cp ~/.lisp/pmdb.lisp vendor/pmdb.lisp
vendor/quickutils.lisp: vendor/quickutil vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load "make-quickutils.lisp"  --non-interactive
vendor/quickutil:
	ln -sf ~/Workspace/quickutil vendor/quickutil
cl-classified:
	mkdir -p vendor/cl-classified
	cp ~/Workspace/cl-encrypted/classified.asd                         vendor/cl-classified/
	cp ~/Workspace/cl-encrypted/encrypted.lisp                         vendor/cl-classified/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlsyntax.lisp                vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlutils-package.lisp         vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlutils.lisp                 vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/net.matteolandi.utils.asd    vendor/cl-classified/vendor/ml/

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
test-sbcl: $(lisps)
	sbcl --noinform --quit \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

.PHONY: test-ros
test-ros: $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

# Report warnings ------------------------------------------------------------
.PHONY: report-warnings
report-warnings: report-warnings-sbcl

.PHONY: report-warnings-sbcl
report-warnings-sbcl: $(lisps)
	sbcl --noinform --quit \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp"

.PHONY: report-warnings-ros
report-warnings-ros: $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp"
