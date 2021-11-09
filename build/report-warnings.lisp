(defun list-dependencies (system)
  "Return SYSTEM dependencies.
If a dependency is part of the subsystem (i.e. they have the same .asd file),
they are not included but their dependencies are."
  (let ((root-location (asdf:system-source-file (asdf:find-system system))))
    (labels ((list-deps-recursively (deps)
               (when deps
                 (let* ((dep (first deps))
                        (location (asdf:system-source-file (asdf:find-system dep))))
                   (if (and location
                            (uiop:pathname-equal location root-location))
                       (append (list-deps-recursively
                                (asdf:system-depends-on (asdf:find-system dep)))
                               (list-deps-recursively (rest deps)))
                       (cons dep (list-deps-recursively (rest deps))))))))
      (let ((all-deps (asdf:system-depends-on (asdf:find-system system))))
        (delete-duplicates (list-deps-recursively all-deps) :test #'string=)))))

(defun redefinition-p (condition)       ; From Slynk.
  (and (typep condition 'style-warning)
       (every #'char-equal "redefin" (princ-to-string condition))))

(defun load-system-silently (system)
  (uiop:with-null-output (null-output)
    (let ((*standard-output* null-output))
      (asdf:load-system system))))

(defun compilation-conditions (system)
  (mapc #'load-system-silently (list-dependencies system))
  (let ((conditions '()))
    (handler-bind ((warning (lambda (c)
                              (unless (redefinition-p c)
                                (push c conditions)))))
      (asdf:load-system system :force t))
    (let ((report (mapcar (lambda (c) (format nil "~a~%" c))
                          (nreverse conditions))))
      (when report
        (format t "~a~&Found ~a warnings when loading ~s."
                report (length report) system)
        (uiop:quit 19)))))


(handler-case (ql:quickload :aoc)
  (error (a) (format t "caught error ~s~%~a~%" a a) (uiop:quit 17)))

(compilation-conditions "aoc")
