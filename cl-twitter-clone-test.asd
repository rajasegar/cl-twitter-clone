(defsystem "cl-twitter-clone-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-twitter-clone"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-twitter-clone"))))
  :description "Test system for cl-twitter-clone"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
