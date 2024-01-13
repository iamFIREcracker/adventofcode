(defpackage #:classified
  (:use #:cl #:mlutils #:3am)
  (:export
   *primary-key*
   *deterministic-key*
   *key-derivation-salt*
   *support-for-unencrypted-data?*
   *frozen-encryption?*
   *store-key-references?*
   *previous-schemes*

   message-serializer
   null-message-serializer
   *message-serializer*

   simple-key-provider
   derived-secret-key-provider
   deterministic-key-provider
   envelope-encryption-key-provider
   *key-provider*

   encrypt
   decrypt

   encryptor
   encrypting-only-encryptor
   null-encryptor
   read-only-null-encryptor
   *encryptor*

   run-without-encryption
   run-protecting-encrypted-data

   encrypted-class
   *object*
   *class*
   *slot*))
(in-package #:classified)

(named-readtables:in-readtable :mlutils-syntax)

#|
      # For each entry it generates an accessor exposing the full name
      DEFAULT_PROPERTIES = {
        encrypted_data_key: "k",
        encrypted_data_key_id: "i",
        compressed: "c",
        iv: "iv",
        auth_tag: "at",
        encoding: "e"
      }

TODO: simpler declarative instantiation for encryptor, key-provider, message-serializer
TODO: how to override the cipher? we could use *cipher-type* w/ make-instance; what about `secret`? can that slot be shared across all the CIPHER instances?
TODO: utf-8 encoding? 🤬
TODO: JSON serialization
TODO: validate and cache *previous-schemes* #20230303-KZYDSTSPM4
TODO: support for find operations
TODO: should be process *SUPPORT-FOR-UNENCRYPTED-DATA?* and *PREVIOUS-SCHEMES* at class definition time, instead of run-time?  It seems probably the safest thing to do, and would not require us to cache encryption schemes (^20230303-KZYDSTSPM4)
|#

(defun bytes (x)
  (if (stringp x)
    (ironclad:ascii-string-to-byte-array x)
    x))
#+#:excluded (bytes "foo")
#+#:excluded (bytes "🤬")

(defun 2string (bytes)
  (coerce (mapcar #'code-char (coerce bytes 'list))
          'string))
#+#:excluded (2string (bytes "foo"))

(defun mklist (x) (if (listp x) x (list x)))

(defun ubarray->base64 (array)
  (base64:usb8-array-to-base64-string array))

(defun base64->ubarray (string)
  (base64:base64-string-to-usb8-array string))


(defparameter *primary-key*
  '("EGY8WhulUOXixybod7ZWwMIL68R9o5kC")
  "Primary key to use to encrypt / decrypt data.
  Can either be a single value, i.e., a string, or a list of values to support
  keys rotation; in which case:
  - All the keys in the list will be used to decrypt data
  - The last key only will be used to encrypt new data.")

(defparameter *deterministic-key* "aPA5XyALhf75NNnMzaspW7akTfZp0lPY"
  "Key used while implementing deterministic encryption.
  The system does not support key rotation for determnistic encryption,
  so this needs to be set to a string.")

(defparameter *key-derivation-salt* "xEY0dt6TZcAMg52K7O84wYzkjvbA62Hz"
  "Salt used while deriving keys from passwords, or other information")

(defparameter *support-for-unencrypted-data?* nil
  "What the library should do when decrypting slots which is not encrypted.

  If NIL, i.e., the default, the library will error out when trying to decrypt
  data that does not look like it was encrypted before.  If T, upon failing to
  decrypt the data, the library will simply return the data as-is.

  You might want to set this to T, temporarily, as you encrypt data in the
  brackground; but the moment you are done with the first pass of encryption,
  better to set this back to NIL.

  Note: this is evaluated at run-time, i.e., when trying to decrypt data, and
  not at class redefinition time.
  ")

(defparameter *frozen-encryption?* nil
  "Whether the library should allow you to change the values of encrypted
  slots.

  If NIL, i.e., the default, the library will allow its users to update
  encrypted slots just fine.  When set to T, an error will be signalled
  every time a user tried to update an encrypted slot.")

(defparameter *store-key-references?* nil
  "Whether the library should store, or not, a reference to the encryption key
  in the encrypted message itself.

  Storing key references will make for more performant decryption because the
  system can now locate keys directly instead of trying lists of keys. However,
  the price to pay is storage: encrypted data will be a bit bigger.
  ")

(defparameter *key-provider* nil
  "The current KEY-PROVIDER.  Usually initialized with the following:

      (setf *key-provider* (derived-secret-key-provider (mklist *primary-key*)))
  ")

(defparameter *previous-schemes* nil
  "A list of previous encryption schemes (see: ENCRYPTION-SCHEME?) to use, when
  decrypt slots.

  Note: this is evaluated at run-time, i.e., when trying to decrypt data, and
  not at class redefinition time.
  ")


(defun derive-secret-from (secret &optional (salt *key-derivation-salt*))
  (ironclad:pbkdf2-hash-password (bytes secret)
                                 :salt (bytes salt)
                                 :digest :sha256
                                 :iterations (expt 2 16)))
#+#:excluded (derive-secret-from "secret1")
#+#:excluded (derive-secret-from "secret2")



(defclass key ()
  ((secret :initarg :secret
           :initform (error "Cannot initialize without :secret"))
   (metadata :initarg :metadata
             :initform nil))
  (:documentation "An encryption/decryption key.
  It contains secret, i.e., the key itself.
  It also contains additional metadata in the form of a PLIST (e.g.,
  a key used to implement envelope encryption will also contain
  the encrypted data key."))

(defgeneric id (key)
  (:documentation "Returns the key ID, i.e., the first 2 bytes of the SHA1
  digest of SECRET.")
  (:method ((key key))
   (let ((digester (ironclad:make-digest :sha1)))
     (ironclad:update-digest digester (bytes (slot-value key 'secret)))
     (subseq (ironclad:produce-digest digester) 0 2))))
#+#:excluded (id (make-instance 'key :secret "abcdefghijk"))
#+#:excluded (ubarray->base64 *)

(defun derive-key-from (seed &optional (salt *key-derivation-salt*))
  "Create a new KEY by deriving its :secret slot starting from `seed`."
  (make-instance 'key :secret (derive-secret-from seed salt)))
#+#:excluded (derive-key-from "seed")



(defvar *prng* (ironclad:make-prng :fortuna))

(defun generate-random-key (&key (length 32)) (ironclad:random-data length *prng*))
#+#:excluded (generate-random-key :length 32)


(defclass key-provider () ()
  (:documentation "A key provider, i.e., an object implementing the
  KEY-PROVIDER protocol.

  See: ENCRYPTION-KEY, DECRYPTION-KEYS."))

(defgeneric encryption-key (keyprov)
  (:documentation "Returns the key to be used while encrypting new data.

  If *STORE-KEY-REFERENCES?* is T, the id of the key (see ID) will be added to
  the METADATA slot of the key (:id place).")
  (:method :around ((keyprov key-provider))
   (let ((key (call-next-method)))
     (when *store-key-references?*
       (let ((metadata (slot-value key 'metadata)))
         (unless (getf metadata :id)
           (setf (slot-value key 'metadata)
                 (list* :id (id key) metadata)))))
     key)))
#+#:excluded (describe (encryption-key (derived-secret-key-provider (list "secret"))))
#+#:excluded (let ((*store-key-references?* t))
               (describe (encryption-key (derived-secret-key-provider (list "secret")))))
#+#:excluded (let ((*store-key-references?* t)
                   (keyprov (derived-secret-key-provider (list "secret"))))
               (describe (encryption-key keyprov))
               ; this should already find :id set, so do nothing
               (describe (encryption-key keyprov)))
#+#:excluded (let* ((*store-key-references?* t)
                    (keyprov (derived-secret-key-provider (list "secret"))))
               (encrypt *encryptor* "Matteo" :key-provider keyprov))
#+#:excluded (let* ((*store-key-references?* t)
                    (*message-serializer* (make-instance 'null-message-serializer))
                    (*key-provider* (derived-secret-key-provider (list "secret1" "secret2")))
                    (keyprov (envelope-encryption-key-provider)))
               (equalp (getf (encrypt *encryptor* "Matteo" :key-provider keyprov) :id)
                       (id (encryption-key *key-provider*))))

(defgeneric decryption-keys (keyprov msg)
  (:documentation "Returns a list of keys to be decrypting existing data with.

  If encrypted message `msg` contains a reference to its encryption key, then
  only the keys with a matching id will be returned.

  Note: multiple keys _can_ share the same id (see ID) so we cannot just return
  the first matching key but we need to return all the ones matching.")
  (:method :around ((keyprov key-provider) msg)
   (let ((keys (call-next-method))
         (used-key-id (getf msg :id)))
     (if used-key-id
       (remove-if-not (lambda (id) (equalp id used-key-id)) keys :key #'id)
       keys))))
#+#:excluded (let ((keyprov (derived-secret-key-provider (list "secret1" "secret2")))
                   (*message-serializer* (make-instance 'null-message-serializer))
                   (*store-key-references?* t))
               (decryption-keys keyprov (encrypt *encryptor* "Matteo" :key-provider keyprov)))
#+#:excluded (let* ((*store-key-references?* t)
                    (*message-serializer* (make-instance 'null-message-serializer))
                    (*key-provider* (derived-secret-key-provider (list "secret1" "secret2")))
                    (keyprov (envelope-encryption-key-provider)))
               (decryption-keys keyprov (encrypt *encryptor* "Matteo" :key-provider keyprov)))

(defclass simple-key-provider (key-provider)
  ((keys :initarg :keys
         :initform (error "Cannot initialize without :keys"))
   encryption-key
   decryption-keys)
  (:documentation "A KEY-PROVIDER initialized with a list of KEYS.

  ENCRYPTION-KEY will retrun the last key of the list.  DECRYPTION-KEYS will
  instead return the full list."))

(defun list-of-keys? (x)
  (and (consp x)
       (every (lambda (e) (subtypep (type-of e) 'key)) x)))

(deftype list-of-keys () `(satisfies list-of-keys?))

(defmethod initialize-instance :after ((self simple-key-provider) &key keys)
  (check-type keys list-of-keys)
  (setf (slot-value self 'encryption-key) (last-elt keys)
        (slot-value self 'decryption-keys) keys))
#+#:excluded (make-instance 'key-provider :keys nil)
#+#:excluded (make-instance 'key-provider :keys (list "foo"))
#+#:excluded (make-instance 'key-provider :keys (list (make-instance 'key :secret "")))

(defun simple-key-provider (keys)
  (make-instance 'simple-key-provider
                 :keys (mapcar #'derive-key-from (or keys (mklist *primary-key*)))))


(defmethod encryption-key ((self simple-key-provider)) (slot-value self 'encryption-key))

(defmethod decryption-keys ((self simple-key-provider) msg) (slot-value self 'decryption-keys))


(defclass derived-secret-key-provider (simple-key-provider) ()
  (:documentation "SIMPLE-KEY-PROVIDER whose keys are derived from :keys.

  *PRIMARY-KEYS* will be used if :keys was empty.

  See DERIVE-KEY-FROM"))

(defmethod initialize-instance :around ((self derived-secret-key-provider) &key keys)
  (call-next-method self :keys (mapcar #'derive-key-from (or keys (mklist *primary-key*)))))
#+#:excluded (make-instance 'derived-secret-key-provider :keys (mklist *primary-key*))
#+#:excluded (make-instance 'derived-secret-key-provider :keys (list "foo" "bar"))

(defun derived-secret-key-provider (seeds)
  (make-instance 'derived-secret-key-provider :keys seeds))
#+#:excluded (describe (derived-secret-key-provider (list "seed1" "seed2")))


(defun deterministic-key-provider ()
  "Creates a SIMPLE-KEY-PROVIDER whose single key is derived from
  *DETERMINISTIC-KEY*.

  Will signal an error if the value behind *DETERMINISTIC-KEY* was not
  a STRING."
  (unless (stringp *deterministic-key*)
    (error "Deterministic encryption keys can't be rotated"))
  (derived-secret-key-provider (list *deterministic-key*)))
#+#:excluded (describe (deterministic-key-provider))


(defclass envelope-encryption-key-provider (key-provider) ()
  (:documentation "A KEY-PROVIDER implementing a basic envelope encryption
  strategy.

  First of, it internally sets PRIMARY-KEY-PROVIVER with *KEY-PROVIDER*.

  Then, while encrypting:

  - It generates a new random data encryption key for each encryption
    operation.
  - It encrypts this reandomly generated encryption key using
    *KEY-PROVIDER*'s ENCRYPTION-KEY.
  - Stores tne encrypted random key inside the metadata slot, behind :ek.

  When decrypting:

  - It pulls the encrypted key from the encrypted message (i.e., :ek).
  - Decrypts it using any of *KEY-PROVIDER*'s DECRYPTION-KEYS."))


(defmethod encryption-key ((self envelope-encryption-key-provider))
  (let* ((random-secret (generate-random-key))
         (encrypted-data-key (encrypt-data-key random-secret)))
    (make-instance 'key
                   :secret random-secret
                   :metadata (list* :ek encrypted-data-key
                                    (if *store-key-references?*
                                      (list :id (id (encryption-key *key-provider*))))))))

(defun encrypt-data-key (data-key)
  (%encrypt data-key (slot-value (encryption-key *key-provider*) 'secret)))
#+#:excluded (describe (encryption-key (envelope-encryption-key-provider)))

(defun contains-encrypted-data-key? (x)
  (and (consp x)
       (getf x :ek)))

(deftype contains-encrypted-data-key ()
  `(satisfies contains-encrypted-data-key?))

;; KEY-PROVIDER has an :AROUND method responsible for filtering decryption keys
;; based on the encryption key id defined inside the message (if any).  The
;; thing is, with envelope encryption, :ID refers to the key used to encrypt
;; the data key, not the message itself, so if we don't override the base
;; :AROUND method, it will end up filtering out our decrypted encryption key.
;; NOTE: this only works if we can afford not to call CALL-NEXT-METHOD.
(defmethod decryption-keys :around ((keyprov envelope-encryption-key-provider) msg)
  (let ((secret (decrypt-data-key msg)))
    (list (make-instance 'key :secret secret))))

;; The following error is signaled if we don't define a primary method of ENVELOPE-ENCRYPTION-KEY-PROVIDER:
;;
;;   There is no primary method for the generic function
;;     #<STANDARD-GENERIC-FUNCTION COMMON-LISP-USER::DECRYPTION-KEYS (4)>
(defmethod decryption-keys ((keyprov envelope-encryption-key-provider) msg)
  (error "Should never reach here"))

(defun decrypt-data-key (msg)
  (check-type msg contains-encrypted-data-key)
  (let ((encrypted-data-key (getf msg :ek)))
    (%decrypt encrypted-data-key
              (mapcar (lambda (key) (slot-value key 'secret))
                      (decryption-keys *key-provider* msg)))))
#+#:excluded (decryption-keys (envelope-encryption-key-provider)
                              (list :ek (encrypt-data-key "secret")))

(defun envelope-encryption-key-provider ()
  (make-instance 'envelope-encryption-key-provider))
#+#:excluded (envelope-encryption-key-provider)


;; Encryption protocol
(defgeneric encrypt (self plaintext &key &allow-other-keys)
  (:documentation "Encrypts `plaintext`.  What does this mean, in practice,
  depends on the objext, `self`, that this method is dispatched on.

  Part of the ENCRYPTION protocol."))

(defgeneric decrypt (self encrypted &key &allow-other-keys)
  (:documentation "Decrypts `encrypted`.  What does this mean, in practice,
  depends on the object, `self`, that this method is dispatched on.

  Part of the ENCRYPTION protocol."))


(defclass cipher () ()
  (:documentation "The algorithm used for encrypting and decrypting data."))


(defun aes-256-gcm (secret &key deterministic &allow-other-keys)
  (make-instance 'aes-256-gcm :secret secret :deterministic deterministic))
#+#:excluded (aes-256-gcm "foo")
#+#:excluded (aes-256-gcm "foo" :deterministic t)
#+#:excluded (aes-256-gcm "foo" :deterministic t :key-provider "foo")

(defclass aes-256-gcm (cipher)
  ((secret :initarg :secret
           :initform (error "Cannot initialize without :secret"))
   (deterministic :initarg :deterministic
                  :initform nil))
  (:documentation "AES-256-GCM cipher algorithm.  Implements the ENCRYPTION
  protocol.

  Supports both deterministic, and non deterministic encryption (default); for
  non deterministic encryption it will generate a random initialization vector
  (IV); for deterministic encryption instead, it will derive IV from the
  plaintext message."))

(defmethod encrypt ((cipher aes-256-gcm) plaintext &key &allow-other-keys)
  (let* ((iv (generate-iv cipher plaintext))
         (mode (get-auth-mode (slot-value cipher 'secret) iv))
         (bytes (bytes plaintext)))
    (list :c (ironclad:encrypt-message mode bytes)
          :i iv
          :g (ironclad:produce-tag mode))))
#+#:excluded (encrypt (aes-256-gcm *primary-key*)
                      "foo")
#+#:excluded (encrypt (aes-256-gcm (car (mklist *primary-key*))
                                   :deterministic t)
                      "foo")

(defun get-hmac (key) (ironclad:make-hmac (bytes key) :sha256))
#+#:excluded (get-hmac *deterministic-key*)

(defmethod generate-iv ((cipher aes-256-gcm) plaintext)
  (if (slot-value cipher 'deterministic)
    (deterministic-iv plaintext (slot-value cipher 'secret))
    (random-iv)))
#+#:excluded (generate-iv (aes-256-gcm "secret") "foo")
#+#:excluded (generate-iv (aes-256-gcm "secret" :deterministic t) "foo")

(defun deterministic-iv (plaintext key)
  (let ((hmac (get-hmac key))
        (bytes (bytes plaintext)))
    (ironclad:update-hmac hmac bytes)
    (subseq (ironclad:hmac-digest hmac) 0 32)))
#+#:excluded (deterministic-iv "foo" *deterministic-key*)

(defun random-iv () (ironclad:random-data 32 *prng*))
#+#:excluded (random-iv)

(defun get-auth-mode (key iv &optional tag)
  (ironclad:make-authenticated-encryption-mode :gcm
                                               :cipher-name :aes
                                               :tag tag
                                               :key (bytes key)
                                               :initialization-vector iv))
#+#:excluded (get-auth-mode (car *primary-key*) (random-iv))


(defun aes-256-gcm-encrypted-msg? (x)
  (and (consp x)
       (getf x :c) ; ciphertext
       (getf x :i) ; initialization vector
       (getf x :g) ; tag
       ))

(deftype aes-256-gcm-encrypted-msg ()
  `(satisfies aes-256-gcm-encrypted-msg?))

(defmethod decrypt ((cipher aes-256-gcm) (msg list) &key &allow-other-keys)
  (check-type msg aes-256-gcm-encrypted-msg)
  (destructuring-bind (&key c i g &allow-other-keys) msg
    (let ((ciphertext c) (iv i) (tag g)
          (key (slot-value cipher 'secret)))
      (let ((mode (get-auth-mode key iv tag)))
        (let ((decrypted (ironclad:decrypt-message mode ciphertext)))
          (2string decrypted))))))
#+#:excluded (let ((cipher (aes-256-gcm (car (mklist *primary-key*)))))
               (decrypt cipher (encrypt cipher "foo")))
#+#:excluded (let ((cipher (aes-256-gcm (car (mklist *primary-key*))
                                        :deterministic t)))
               (decrypt cipher (encrypt cipher "foo")))


(defun %encrypt (plaintext secret &rest cipher-options)
  (encrypt (apply #'aes-256-gcm secret cipher-options)
           plaintext))


(define-condition cannot-unwrap-decrypted-msg () ())

(defun %decrypt (msg secrets &rest cipher-options)
  (dolist (secret secrets)
    (flet ((try-next-or-bail (c)
             (if (eq (last-elt secrets) secret)
               (error c))))
      (handler-case
          (let ((cipher (apply #'aes-256-gcm secret cipher-options)))
            (return (decrypt cipher msg)))
        (ironclad:bad-authentication-tag (c) (try-next-or-bail c))))))

(defparameter *headers* (list

                          :ek ; encrypted data key (envelope encryption)
                          :id ; encryption key reference id
                          :c  ; ciphertext
                          :i  ; initialization vector
                          :g  ; auth tag

                          ))

(defclass message-serializer () ()
  (:documentation "A message serializer implements the MESSAGE-SERIALIZER
  protocol.

  See: SERIALIZE, and DESERIALIZE."))

(defgeneric serialize (serializer x)
  (:documentation "Serialize `x`.

  `x` is expected to be a plist; the property values, if arrays of bytes, will
  be base64-encoded; the end-result will be JSON-stringified.")
  (:method ((serializer message-serializer) x)
   (loop for k in *headers* for v = (getf x k)
         when v nconc (list k (etypecase v
                                (array (ubarray->base64 v))
                                (list (serialize serializer v)))))))
#+#:excluded (serialize (make-instance 'message-serializer)
                        (%encrypt "foo" (car (mklist *primary-key*))))
#+#:excluded (serialize (make-instance 'message-serializer)
                        (destructuring-bind (&key c i g)
                            (%encrypt "foo" (car (mklist *primary-key*)))
                          (list :g g :i i :c c)))

(defgeneric deserialize (serializer x)
  (:documentation "Deserializes `x`.

  `x` is expected to be a JSON-parsable; the parsed object should then be
  a plist with base64 encoded byte arrays as values.")
  (:method ((serializer message-serializer) x)
   (loop for k in *headers* for v = (getf x k)
         when v nconc (list k
                            (etypecase v
                              (string (base64->ubarray v))
                              (list (deserialize serializer v)))))))
#+#:excluded (let ((serializer (make-instance 'message-serializer)))
               (deserialize serializer
                            (serialize serializer
                                       (%encrypt "foo" (car (mklist *primary-key*))))))
#+#:excluded (let ((serializer (make-instance 'message-serializer)))
               (deserialize serializer
                            (destructuring-bind (&key c i g)
                                (serialize serializer (%encrypt "foo" (car (mklist *primary-key*))))
                              (list :g g :i i :c c))))


(defclass null-message-serializer (message-serializer) ()
  (:documentation "A MESSAGE-SERIALIZER that does not serialize nor
  deserialize.  It will just return the passed in values. "))

(defmethod serialize ((serializer null-message-serializer) x)
  x)

(defmethod deserialize ((serializer null-message-serializer) x)
  x)


(defparameter *message-serializer* (make-instance 'null-message-serializer)
  "The current MESSAGE-SERIALIZER, i.e. an instance of NULL-MESSAGE-SERIALIZER.")


(defclass encryptor () ()
  (:documentation "An encrytor implements the ENCRYPTION protocol.

  It interacts with a KEY-PROVIDER for getting the keys (defaults to
  *KEY-PROVIDER*); delegates to the configured CIPHER for the actual
  encryption/decryption steps; delegates to the configured MESSAGE-SERIALIZER
  (defatuls to *MESSAGE-SERIALIZER*) to convert the encrypted message into
  a serializable form."))

(defmethod encrypt ((encryptor encryptor) (plaintext string)
                    &key
                    (message-serializer *message-serializer*)
                    (key-provider *key-provider*)
                    cipher-options)
  (let* ((key (encryption-key key-provider)))
    (serialize message-serializer
               (append (apply '%encrypt plaintext (slot-value key 'secret) cipher-options)
                       (slot-value key 'metadata)))))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*))))
               (encrypt (make-instance 'encryptor) "Matteo"))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*))))
               (encrypt (make-instance 'encryptor)
                        "Matteo"
                        :cipher-options '(:deterministic t)))
#+#:excluded (encrypt (make-instance 'encryptor)
                      "Matteo"
                      :key-provider (envelope-encryption-key-provider))
#+#:excluded (encrypt (make-instance 'encryptor)
                      "Matteo"
                      :key-provider (envelope-encryption-key-provider)
                      :message-serializer (make-instance 'message-serializer))

(defmethod decrypt ((encryptor encryptor) msg
                    &key (key-provider *key-provider*)
                    (message-serializer *message-serializer*)
                    cipher-options)
  (setf msg (deserialize message-serializer msg))
  (let ((keys (decryption-keys key-provider msg)))
    (if (not keys)
      (error "No decryption key found")
      (apply '%decrypt
             msg
             (mapcar (lambda (key) (slot-value key 'secret)) keys)
             cipher-options))))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*)))
                   (encryptor (make-instance 'encryptor)))
               (decrypt encryptor
                        (encrypt encryptor "Matteo")))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*)))
                   (encryptor (make-instance 'encryptor))
                   (cipher-options (list :deterministic t)))
               (decrypt encryptor
                        (encrypt encryptor "Matteo" :cipher-options cipher-options)))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*)))
                   (encryptor (make-instance 'encryptor)))
               (decrypt encryptor
                        (encrypt encryptor
                                 "Matteo"
                                 :key-provider (envelope-encryption-key-provider))
                        :key-provider (envelope-encryption-key-provider)))
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*)))
                   (encryptor (make-instance 'encryptor))
                   (message-serializer (make-instance 'message-serializer)))
               (decrypt encryptor
                        (encrypt encryptor
                                 "Matteo"
                                 :key-provider (envelope-encryption-key-provider)
                                 :message-serializer message-serializer)
                        :key-provider (envelope-encryption-key-provider)
                        :message-serializer message-serializer))


(defclass encrypting-only-encryptor (encryptor) ()
  (:documentation "An encryptor that encrypts data but does not decrypt it."))

(defmethod decrypt ((encryptor encrypting-only-encryptor) msg &key &allow-other-keys)
  msg)
#+#:excluded (let ((*key-provider* (derived-secret-key-provider (mklist *primary-key*)))
                   (encryptor (make-instance 'encrypting-only-encryptor)))
               (decrypt encryptor
                        (encrypt encryptor "Matteo")))

(defparameter *encryptor* (make-instance 'encrypting-only-encryptor)
  "The current ENCRYPTOR, i.e. an instance of ENCRYPTING-ONLY-ENCRYPTOR.

  See also: ENCRYTPOR, NULL-ENCRYPTOR, and READ-ONLY-NULL-ENCRYPTOR.
  ")


(defclass null-encryptor (encryptor) ()
  (:documentation "An encryptor that does not encrypt nor decrypt.
  It will just return the passed in values."))

(defmethod encrypt ((encryptor null-encryptor) plaintext &key &allow-other-keys)
  plaintext)

(defmethod decrypt ((encryptor null-encryptor) msg &key &allow-other-keys)
  msg)


(defclass read-only-null-encryptor (null-encryptor) ()
  (:documentation "A NULL-ENCRYPTOR that raises an error when asked to encrypt data.
  Useful when debugging a production environment, where you don't want
  to accidentally decrypt sensitive data nor accidentally re-encrypt it."))

(defmethod encrypt ((encryptor read-only-null-encryptor) plaintext &key &allow-other-keys)
  (error "This encryptor is read-only"))


(defun build-key-provider (&optional key deterministic)
  (if key
    (derived-secret-key-provider (list key))
    (if deterministic
      (deterministic-key-provider)
      (error "Cannot build key provider unless :key or :deterministic were specified"))))


(defparameter *null-encryptor* (make-instance 'null-encryptor)
  "A NULL-ENCRYPTOR instance.")

(defmacro run-without-encryption (&body body)
  `(let ((*encryptor* *null-encryptor*))
     ,@body))


(defmacro run-protecting-encrypted-data (&body body)
  `(let ((*encryptor* (make-instance 'encrypting-only-encryptor))
         (*frozen-encryption?* t))
     ,@body))



;;; CLOS

(defclass encrypted-class (standard-class) ())

(defmethod closer-mop:validate-superclass
    ((sub encrypted-class) (sup standard-class))
  t)


;; subclass of standard-direct-slot-definition
(defclass encrypted-direct-slot-definition
    (closer-mop:standard-direct-slot-definition)
  ((encrypted
     :initarg :encrypted
     :initform nil
     :documentation "A slot option indicating that the slot is to be encrypted.")))

;; letting CLOS know that you want to use your custom
;; class whenever :encrypted is non null
;; otherwise, you'll just use the standard slot definition class.
(defmethod closer-mop:direct-slot-definition-class
    ((class encrypted-class) &key encrypted &allow-other-keys)
  (if encrypted
    'encrypted-direct-slot-definition
    (call-next-method)))


;; a custom effective slot definition class
(defclass encrypted-effective-slot-definition
    (closer-mop:standard-effective-slot-definition)
  ((encrypted :initarg :encrypted :initform nil)))

;; let clos know what you're doing
(defmethod closer-mop:effective-slot-definition-class
    ((class encrypted-class) &rest initargs)
  (declare (ignore initargs))
  'encrypted-effective-slot-definition)


(defparameter *object* nil
  "The object, whose slot is currently being encrypted/decrypted.
  NIL when a class encryption scheme is currently getting finalized.")
(defparameter *class* nil
  "The class of object (see *OBJECT*) whose slot is currently getting
  encrypted/decrypted; or the class whose slot's encryption scheme is getting
  finalized.")
(defparameter *slot* nil
  "The slot of object (see *OBJECT*) whose value is currently getting
  encrypted/decrypted; or the slot whose encryption scheme is getting
  finalized.")


;; specialize on encrypted-class
(defmethod closer-mop:compute-effective-slot-definition
    ((class encrypted-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (let ((encrypted-slot-def
            (find-if (lambda (ds) (typep ds 'encrypted-direct-slot-definition))
                     direct-slots)))
      (when encrypted-slot-def
        (let ((*object* nil)
              (*class* class)
              (*slot* encrypted-slot-def))
          (setf (slot-value effective-slot 'encrypted)
                (finalize-encryption-scheme class encrypted-slot-def)))))
    effective-slot))

(defun encryption-scheme? (x &optional (can-have-previous? t))
  (or (and x (atom x))
      (and (let ((v (getf x :encryptor)))
             (or (not v) (symbolp v)))
           (let ((v (getf x :message-serializer)))
             (or (not v) (symbolp v)))
           (let ((v (getf x :key-provider)))
             (or (not v) (symbolp v)))
           (let ((v (getf x :key)))
             (or (not v) (string v)))
           (let ((v (getf x :deterministic)))
             (or (not v) (atom v)))
           (let ((v (getf x :previous)))
             (or (not v)
                 (and can-have-previous?
                      (every (lambda (scheme) (encryption-scheme? scheme nil)) v))))
           )))

(deftype encryption-scheme () `(satisfies encryption-scheme?))


(defun finalize-encryption-scheme (class slot
                                         &optional
                                         (scheme (slot-value slot 'encrypted))
                                         &aux
                                         final)
  (check-type scheme encryption-scheme)
  (when (and scheme (atom scheme)) (setf scheme nil))
  (destructuring-bind (&key encryptor message-serializer
                            key-provider key deterministic previous) scheme
    (when (and key-provider key)
      (error ":KEY-PROVIDER and :KEY can't be used simultaneously"))
    (when encryptor
      (setf (getf final :encryptor)
            (finalize-encryption-scheme-encryptor encryptor)))
    (when message-serializer
      (setf (getf final :message-serializer)
            (finalize-encryption-scheme-message-serializer message-serializer)))
    (when (or key-provider key deterministic)
      (setf (getf final :key-provider)
            (if key-provider
              (finalize-encryption-scheme-key-provider key-provider)
              (build-key-provider key deterministic))))
    (when deterministic
      (setf (getf final :deterministic) t))
    ; XXX can you nest _previous_? looks like the check is done with (check-type scheme encryption-scheme)
    (when previous
      (setf (getf final :previous)
            (mapcar (lambda (scheme) (finalize-encryption-scheme class slot scheme))
                    previous))))
  (or final (list :deterministic nil)))
#+#:excluded (finalize-encryption-scheme 'class 'slot t)
#+#:excluded (finalize-encryption-scheme 'class 'slot '(:deterministic t))
#+#:excluded (progn
               (defun my-encryptor (class slot)
                 (print (list :class class :slot slot))
                 'my-encryptor)
               (finalize-encryption-scheme 'class 'slot '(:encryptor my-encryptor)))
#+#:excluded (progn
               (defun my-message-serializer (class slot)
                 (print (list :class class :slot slot))
                 'my-message-serializer)
               (finalize-encryption-scheme 'class 'slot '(:encryptor my-message-serializer)))
#+#:excluded (progn
               (defun my-key-provider (class slot)
                 (print (list :class class :slot slot))
                 'my-key-provider)
               (finalize-encryption-scheme 'class 'slot '(:key-provider my-key-provider)))
#+#:excluded (progn
               (finalize-encryption-scheme 'class 'slot '(:previous ((:deterministic t)))))

(defun finalize-encryption-scheme-encryptor (encryptor)
  (if (consp encryptor)
    (if (eq (car encryptor) 'lambda)
      (let ((encryptor (funcall (eval encryptor))))
        (check-type encryptor encryptor)
        encryptor)
      (destructuring-bind (type &rest initargs) encryptor
        (apply #'make-instance type (mapcar #'eval initargs))))
    (let ((encryptor (make-instance encryptor)))
      (check-type encryptor encryptor)
      encryptor)))
#+#:excluded (finalize-encryption-scheme-encryptor '(lambda () nil))
#+#:excluded (finalize-encryption-scheme-encryptor '(lambda () *encryptor*))
#+#:excluded (finalize-encryption-scheme-encryptor '(lambda () (make-instance 'encrypting-only-encryptor)))
#+#:excluded (finalize-encryption-scheme-encryptor '(encrypting-only-encryptor))
#+#:excluded (finalize-encryption-scheme-encryptor 'encrypting-only-encryptor)

(defun finalize-encryption-scheme-message-serializer (message-serializer)
  (if (consp message-serializer)
    (if (eq (car message-serializer) 'lambda)
      (let ((serializer (funcall (eval message-serializer))))
        (check-type serializer message-serializer)
        serializer)
      (destructuring-bind (type &rest initargs) message-serializer
        (apply #'make-instance type (mapcar #'eval initargs))))
    (let ((serializer (make-instance message-serializer)))
      (check-type serializer message-serializer)
      serializer)))
#+#:excluded (finalize-encryption-scheme-message-serializer '(lambda () nil))
#+#:excluded (finalize-encryption-scheme-message-serializer '(lambda () *message-serializer*))
#+#:excluded (finalize-encryption-scheme-message-serializer '(lambda () (make-instance 'message-serializer)))
#+#:excluded (finalize-encryption-scheme-message-serializer '(message-serializer))
#+#:excluded (finalize-encryption-scheme-message-serializer 'message-serializer)

(defun finalize-encryption-scheme-key-provider (key-provider)
  (if (consp key-provider)
    (if (eq (car key-provider) 'lambda)
      (let ((keyprov (funcall (eval key-provider))))
        (check-type keyprov key-provider)
        keyprov)
      (destructuring-bind (type &rest initargs) key-provider
        (apply #'make-instance type (mapcar #'eval initargs))))
    (let ((keyprov (make-instance key-provider)))
      (check-type keyprov key-provider)
      keyprov)))
#+#:excluded (finalize-encryption-scheme-key-provider '(lambda () nil))
#+#:excluded (finalize-encryption-scheme-key-provider '(lambda () *key-provider*))
#+#:excluded (finalize-encryption-scheme-key-provider '(lambda () (make-instance 'derived-secret-key-provider)))
#+#:excluded (finalize-encryption-scheme-key-provider '(derived-secret-key-provider :keys (list "abc")))
#+#:excluded (finalize-encryption-scheme-key-provider 'derived-secret-key-provider)


;; specialize on setf b/c you only want to encrypt on write
(defmethod (setf closer-mop:slot-value-using-class) :around
    (new-value class object (slot encrypted-effective-slot-definition))
  (let ((scheme (slot-value slot 'encrypted)))
    (if (not scheme)
      (call-next-method)
      (let* ((name (closer-mop:slot-definition-name slot))
             (encrypted (encrypt-slot object name new-value scheme)))
        (call-next-method encrypted class object slot)))))

(defgeneric encrypt-slot (object slot new-value scheme))
(defmethod encrypt-slot (object slot (new-value string) scheme)
  #+#:excluded (print (list :encrypting object :slot slot :from new-value :using-scheme scheme))
  (if *frozen-encryption?*
    (error "Cant modify encrypted attributes when frozen")
    (let ((*object* object)
          (*class* (class-of object))
          (*slot* slot))
      (%encrypt-slot new-value scheme))))

(defun %encrypt-slot (new-value scheme)
  (destructuring-bind (&key (encryptor *encryptor*)
                            (message-serializer *message-serializer*)
                            (key-provider *key-provider*)
                            &allow-other-keys)
      scheme
    (encrypt encryptor new-value
             :message-serializer message-serializer
             :key-provider key-provider
             :cipher-options scheme)))

;; decrypt on read
(defmethod closer-mop:slot-value-using-class :around
    (class object (slot encrypted-effective-slot-definition))
  (let ((value (call-next-method))
        (scheme (slot-value slot 'encrypted)))
    (if scheme
      (let* ((name (closer-mop:slot-definition-name slot)))
        (decrypt-slot object name value scheme))
      value)))

(defmethod decrypt-slot (object slot value scheme)
  #+#:excluded (print (list :decrypting object :slot slot :using-scheme scheme))
  (let ((schemes (configured-decryption-schemes (class-of object) slot scheme)))
    (dolist (scheme schemes)
      (flet ((try-next-or-bail (c)
               (if (eq (last-elt schemes) scheme)
                 (error c))))
        (handler-case
            (return (%decrypt-slot value scheme))
          (ironclad:bad-authentication-tag (c) (try-next-or-bail c))
          (cannot-unwrap-decrypted-msg (c) (try-next-or-bail c)))))))

(defun configured-decryption-schemes (class slot scheme)
  (let ((schemes (cons scheme (getf scheme :previous))))
    (when *previous-schemes*
      (setf schemes (append schemes
                            (mapcar (lambda (s)
                                      (finalize-encryption-scheme class slot s))
                                    *previous-schemes*))))
    (when *support-for-unencrypted-data?*
      (setf schemes (append schemes (list (null-encryption-scheme)))))
    schemes))

(defun null-encryption-scheme () (list :encryptor *null-encryptor*))
#+#:excluded (null-encryption-scheme)


(defun %decrypt-slot (value scheme)
  (destructuring-bind (&key (encryptor *encryptor*)
                            (message-serializer *message-serializer*)
                            (key-provider *key-provider*)
                            &allow-other-keys)
      scheme
    (decrypt encryptor value
             :message-serializer message-serializer
             :key-provider key-provider
             :cipher-options scheme)))
