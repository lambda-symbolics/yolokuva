(asdf:defsystem #:yolokuva
  :description "Recognize, measure, and size images for multimodal model prompts."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "formats")
                             (:file "limits")
                             (:file "data-url"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:yolokuva/tests))))

(asdf:defsystem #:yolokuva/opticl
  :description "Decode and resize images for prompts through opticl."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :depends-on (#:yolokuva #:opticl)
  :components ((:module "src"
                :components ((:file "prepare")))))

(asdf:defsystem #:yolokuva/tests
  :description "Tests for yolokuva."
  :depends-on (#:yolokuva #:yolokuva/opticl)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:yolokuva/tests '#:run-tests)))
