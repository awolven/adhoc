(defsystem adhoc-tests
  :description "ADHOC tests"
  :depends-on (:adhoc :fiveam)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "tests/package")
   (:file "tests/tests")))
