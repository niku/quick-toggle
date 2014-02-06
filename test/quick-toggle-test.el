(defun with-fixtures (body)
  (unwind-protect
      (progn
        ; set up part
        (let ((rules '(("spec/lib/\\(.+\\)_spec\.rb" . "lib/\\1.rb")
                       ("lib/\\(.+\\)\.rb" . "spec/lib/\\1_spec.rb"))))
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
              (quick-toggle-find-rule "/home/niku/projects/foo/lib/foo.rb" rules)
              (cons "lib/\\(.+\\)\.rb" "spec/lib/\\1_spec.rb"))))))

(ert-deftest quick-toggle-find-rule:when-matched-second ()
  (with-fixtures
   (lambda ()
     (should (equal
              (quick-toggle-find-rule "/home/niku/projects/foo/spec/lib/foo_spec.rb" rules)
              (cons "spec/lib/\\(.+\\)\_spec\.rb" "lib/\\1.rb"))))))

(ert-deftest quick-toggle-find-rule:when-unmatched ()
  (with-fixtures
   (lambda ()
     (should (equal
              (quick-toggle-find-rule "/home/niku/projects/foo/ext/bar.rb" rules)
              nil)))))

(ert-deftest quick-toggle-get-dist-pathname:spec-to-lib ()
  (let ((rule (cons "spec/lib/\\(.+\\)_spec\.rb" "lib/\\1.rb")))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-dist-pathname "/home/niku/projects/foo/spec/lib/foo_spec.rb" rule)
                "/home/niku/projects/foo/lib/foo.rb"))))))

(ert-deftest quick-toggle-get-dist-pathname:lib-to-spec ()
  (let ((rule (cons "lib/\\(.+\\)\.rb" "spec/lib/\\1_spec.rb")))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-dist-pathname "/home/niku/projects/foo/lib/foo.rb" rule)
                "/home/niku/projects/foo/spec/lib/foo_spec.rb"))))))

(ert-deftest quick-toggle-get-dist-pathname:given-rule-is-nil ()
  (let ((rule nil))
    (with-fixtures
     (lambda ()
       (should (equal
                (quick-toggle-get-dist-pathname "/home/niku/projects/foo/lib/foo.rb" rule)
                nil))))))

(ert-deftest quick-toggle-buffer ()
  (with-mock
   (mock (buffer-file-name) => "/home/niku/projects/foo/lib/foo.rb")
   (mock (find-file "/home/niku/projects/foo/spec/lib/foo_spec.rb"))
   (with-fixtures
    (lambda ()
      (quick-toggle-buffer)))))
