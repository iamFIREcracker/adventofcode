lisps := $(shell find . -follow -type f \( -iname \*.asd -o -iname \*.lisp \) ! -path "./vendor/*")

SBCL_BIN ?= sbclw
SBCL_ARGS ?= --noinform --dynamic-space-size 4096

all: vendor

# Vendor ----------------------------------------------------------------------
.PHONY: vendor
vendor: quickutils

.PHONY: mlsyntax
mlsyntax:
	mkdir -p vendor/mlsyntax
	cp ~/Workspace/mlutils/mlsyntax.lisp vendor/mlsyntax/

.PHONY: quickutils
quickutils:
	cd vendor && sbclw --noinform --load "make-quickutils.lisp"  --non-interactive

.PHONY: cl-classified
cl-classified:
	mkdir -p vendor/cl-classified
	cp ~/Workspace/cl-encrypted/classified.asd                         vendor/cl-classified/
	cp ~/Workspace/cl-encrypted/encrypted.lisp                         vendor/cl-classified/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlsyntax.lisp                vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlutils-package.lisp         vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/mlutils.lisp                 vendor/cl-classified/vendor/ml/
	cp ~/Workspace/cl-encrypted/vendor/ml/net.matteolandi.utils.asd    vendor/cl-classified/vendor/ml/

# Start -----------------------------------------------------------------------
.PHONY: start
start:
	sbcl-vlime ${SBCL_ARGS} \
		--eval "(ql:quickload :cl-dotenv)" \
		--eval "(.env:load-env #P\"./.env\")" \
		--eval "(pushnew '*default-pathname-defaults* asdf:*central-registry*)" \
		--eval "(ql:quickload \"AOC\")"

# Info ------------------------------------------------------------------------
.PHONY: lisp-info
lisp-info:
	sbclw --noinform --quit \
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
	sbclw ${SBCL_ARGS} --quit \
		--eval "(ql:quickload :cl-dotenv)" \
		--eval "(.env:load-env #P\"./.env\")" \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

.PHONY: test-ros
test-ros: $(lisps)
	ros run -- ${SBCL_ARGS} \
		--eval "(ql:quickload :cl-dotenv)" \
		--eval "(.env:load-env #P\"./.env\")" \
		--load "build/setup.lisp" \
		--load "build/test.lisp"

# Report warnings ------------------------------------------------------------
.PHONY: report-warnings
report-warnings: report-warnings-sbcl

.PHONY: report-warnings-sbcl
report-warnings-sbcl: $(lisps)
	sbclw --noinform --quit \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp"

.PHONY: report-warnings-ros
report-warnings-ros: $(lisps)
	ros run \
		--load "build/setup.lisp" \
		--load "build/report-warnings.lisp"
