(pushnew (merge-pathnames (parse-namestring "vendor/ml/")
           *default-pathname-defaults*)
         asdf:*central-registry*)
(pushnew (merge-pathnames (parse-namestring "vendor/3am/")
           *default-pathname-defaults*)
         asdf:*central-registry*)

(asdf:defsystem #:classified
  :description "XXX"

  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"

  :version "0.0.1"

  :depends-on (
                 #:ml
                 #:3am

                 #:cl-base64
                 #:closer-mop
                 #:ironclad
              )

  :serial t
  :components (

               (:file "encrypted")

               )

  )
