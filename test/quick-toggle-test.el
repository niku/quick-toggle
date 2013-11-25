(require 'ert)

(require 'quick-toggle)

(ert-deftest myfile:addition-test ()
  (should (= (myfile:addition 1 2) 3)))
