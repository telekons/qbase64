;;;; qbase64.asd

(asdf:defsystem #:qbase64
  :description "Fast and flexible base64 encoder and decoder"
  :version "0.3.0"
  :author "Chaitanya Gupta <mail@chaitanyagupta.com>"
  :license "BSD-3-Clause"
  :serial t
  :depends-on ("trivial-gray-streams" "metabang-bind" (:feature :use-qbase64-sse2 "cl-simd"))
  :components ((:file "package")
               (:file "utils")
               (:file "stream-utils")
               (:file "sse2" :if-feature :use-qbase64-sse2)
               (:file "qbase64")))

(asdf:defsystem "qbase64/test"
  :depends-on ("qbase64" "fiveam" "temporary-file")
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam '#:run!
                                      (list (uiop:find-symbol* '#:encoder '#:qbase64-test)
                                            (uiop:find-symbol* '#:decoder '#:qbase64-test))))
  :components ((:file "qbase64-test")))
