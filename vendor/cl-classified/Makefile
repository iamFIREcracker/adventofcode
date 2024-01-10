.PHONY: all
all: ml 3am

.PHONY: ml
ml:
	mkdir -p vendor/ml
	cp ~/.lisp/ml/mlsyntax.lisp         vendor/ml/
	cp ~/.lisp/ml/mlutils-package.lisp  vendor/ml/
	cp ~/.lisp/ml/mlutils.lisp          vendor/ml/
	cp ~/.lisp/ml/ml.asd                vendor/ml/

.PHONY: 3am
3am:
	mkdir -p vendor/3am
	cp ~/Workspace/3am/3am.asd          vendor/3am/
	cp ~/Workspace/3am/3am.lisp         vendor/3am/
