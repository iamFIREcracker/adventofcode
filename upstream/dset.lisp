(defpackage :dset
  (:use :cl)
  (:export
   :make-dset
   :dset-value
   :dset-find
   :dset-union))
(in-package :dset)

(defstruct (dset (:type list)
                 (:constructor make-dset%)
                 (:copier nil))
  value
  rank
  parent)

(defun make-dset (value)
  "Creates a DISJOINT-SET, having `value` as its only element."
  (let ((s (make-dset% :value value :rank 0)))
    (setf (dset-parent s) s)))

(defun dset-find (x)
  "Finds `x`'s root node.

  If `x` is not a root node itself (i.e. `(eq (dset-parent x) x)` is NIL),
  then this function will also update `x`'s PARENT slot accordingly and
  hopefully speed up future access operations."
  (if (eq (dset-parent x) x)
    x
    (setf (dset-parent x) (dset-find (dset-parent x)))))

(defun dset-union (x y)
  "Joins `x` and `y` and return the root of the new set.

  The new root is chosen in order to _minimize_ the overall set ranking:

  - Find `x`'s root node
  - Find `y`'s root node
  - If different, elect as new root the one with the biggest RANK value
  - Otherwise, pick one and increase its RANK value by one

  This should help and keep the tree _balanced_."
  (let ((x-root (dset-find x))
        (y-root (dset-find y)))
    (cond ((> (dset-rank x-root) (dset-rank y-root))
           (setf (dset-parent y-root) x-root))
          ((< (dset-rank x-root) (dset-rank y-root))
           (setf (dset-parent x-root) y-root))
          ((not (eq x-root y-root))
           (incf (dset-rank x-root))
           (setf (dset-parent y-root) x-root)))))
