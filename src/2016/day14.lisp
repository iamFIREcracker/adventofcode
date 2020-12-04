(defpackage :aoc/2016/14 #.cl-user::*aoc-use*)
(in-package :aoc/2016/14)

(defvar *salt* nil)
(defvar *key-stretching* nil)

(defun circular-cache (size &optional initial-element)
  (let ((cache (make-list size :initial-element initial-element)))
    (ncycle cache)))

;; Gutted from irconclad i.e. BYTE-ARRAY-TO-HEX-STRING, but simplified
(defun md5-bytes-string (bytes)
  (let ((hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop :with string = (make-string 32 :element-type 'base-char)
          :for byte :across bytes
          :for index :below 32 :by 2
          :do (setf (aref string index)
                    (aref hexdigits (ldb (byte 4 4) byte))
                    (aref string (1+ index))
                    (aref hexdigits (ldb (byte 4 0) byte)))
          :finally (return string))))

(defun generate-hash (index)
  (let ((string (format nil "~A~D" *salt* index)))
    (dotimes (n (1+ *key-stretching*) string)
      (setf string (md5-bytes-string (md5:md5sum-string string))))))

(defun find-all-quintet-chars (string)
  (mapcar #'(lambda (match) (aref match 0))
          (cl-ppcre:all-matches-as-strings "(.)\\1\\1\\1\\1" string)))

(defun hash (index cache)
  (if-let ((cached (car cache)))
    cached
    (let ((hash (generate-hash index)))
      (setf (car cache) (cons hash (find-all-quintet-chars hash))))))

(defun find-triplet-char (string)
  (when-let ((pos (cl-ppcre:scan "(.)\\1\\1" string)))
    (aref string pos)))

(defun keyp (index cache char)
  (loop :repeat 1000
        :for lcache = cache :then (cdr lcache)
        :for lindex = index :then (1+ lindex)
        :for (hash . quintets) = (hash lindex lcache)
        :thereis (member char quintets)))

(defun solve (salt &key (key-stretching 0))
  (let ((*salt* salt)
        (*key-stretching* key-stretching))
    (loop :for cache = (circular-cache 1001) :then (cdr cache)
          :for index = 0 :then (1+ index)
          :for (hash . ignored) = (hash index cache)
          :for char = (find-triplet-char hash)
          :count (and char (keyp (1+ index) (cdr cache) char)) :into keys
          :when (= keys 64) :return index
          :do (setf (car cache) nil))))

(define-solution (2016 14) (salt first)
  (values
    (solve salt)
    (swallow (solve salt :key-stretching 2016))))

(define-test (2016 14) (16106  22423))
