(asdf:defsystem #:net.matteolandi.utils
  :author "Matteo Landi <matteo@matteolandi.net>"
  :license  "MIT"
  :serial t
  :depends-on (#:named-readtables)
  :components
  (
   (:file "mlutils-package")
   (:file "mlsyntax")
   (:file "mlutils")
   )
  )
