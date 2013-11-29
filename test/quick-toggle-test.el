(defun with-fixtures (body)
  (unwind-protect
      (progn
        ; set up part
        (let ((rules #s(hash-table test equal
                                 data(
                                      "spec/lib/\\(.+\\)_spec\.rb" "lib/\\1.rb"
                                      "lib/\\(.+\\)\.rb" "spec/lib/\\1_spec.rb"
                                      ))))
          ; test part
          (funcall body))
        )
    (
     ; tear down part
     )))

(ert-deftest quick-toggle-find-rule:when-matched-first ()
  (with-fixtures
   (lambda ()
     (should (equal
              (quick-toggle-find-rule rules "/home/niku/projects/foo/lib/foo.rb")
              (cons "lib/\\(.+\\)\.rb" "spec/lib/\\1_spec.rb"))))))

(ert-deftest quick-toggle-find-rule:when-matched-second ()
  (with-fixtures
   (lambda ()
     (should (equal
              (quick-toggle-find-rule rules "/home/niku/projects/foo/spec/lib/foo_spec.rb")
              (cons "spec/lib/\\(.+\\)\_spec\.rb" "lib/\\1.rb"))))))

(ert-deftest quick-toggle-find-rule:when-unmatched ()
  (with-fixtures
   (lambda ()
     (should (equal
              (quick-toggle-find-rule rules "/home/niku/projects/foo/ext/bar.rb")
              nil)))))

(ert-deftest quick-toggle-get-new-pathname:spec-to-lib ()
  (let ((rule (cons "spec/lib/\\(.+\\)_spec\.rb" "lib/\\1.rb")))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-new-pathname rule "/home/niku/projects/foo/spec/lib/foo_spec.rb")
                "/home/niku/projects/foo/lib/foo.rb"))))))

(ert-deftest quick-toggle-get-new-pathname:lib-to-spec ()
  (let ((rule (cons "lib/\\(.+\\)\.rb" "spec/lib/\\1_spec.rb")))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-new-pathname rule "/home/niku/projects/foo/lib/foo.rb")
                "/home/niku/projects/foo/spec/lib/foo_spec.rb"))))))

(ert-deftest quick-toggle-get-new-pathname:given-rule-is-nil ()
  (let ((rule nil))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-new-pathname rule "/home/niku/projects/foo/lib/foo.rb")
                nil))))))

(ert-deftest quick-toggle-buffer ()
  (with-mock
   (stub buffer-file-name => "/home/niku/projects/foo/lib/foo.rb")
   (mock (find-file "/home/niku/projects/foo/lib/foo.rb"))
   (quick-toggle-buffer)))
