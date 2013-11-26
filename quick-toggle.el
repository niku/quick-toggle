;;; quick-toggle.el --- quickly open corresponding file (eg test vs impl).

;; Author: niku <niku@niku.name>
;; URL: https://github.com/niku/quick-toggle
;; Version 0.0.1

;; The MIT License (MIT)

;; Copyright (c) 2013 niku

;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; This package provides the ability to quickly open a corresponding
;; file for the current buffer by using a bi-directional mapping of
;; regular expression pairs. You can select a mapping style from
;; `quick-toggle-mapping-styles' using the `quick-toggle-style' function or set
;; your default style via the `quick-toggle-mapping-style' variable.

;; There are 4 different mapping styles in this version: zentest,
;; rails, and ruby. Feel free to submit more and I'll incorporate
;; them.

;;; Example Mapping (ruby style):
;;
;; blah.rb <-> test_blah.rb
;; lib/blah.rb <-> test/test_blah.rb

(require 'cl-lib)

(defcustom quick-toggle-mapping-styles
  '((zentest . (("app/controllers/\\1.rb" . "test/controllers/\\1_test.rb")
                ("app/views/\\1.rb"       . "test/views/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (rspec   . (("app/models/\\1.rb"      . "spec/models/\\1_spec.rb")
                ("app/controllers/\\1.rb" . "spec/controllers/\\1_spec.rb")
                ("app/views/\\1.rb"       . "spec/views/\\1_spec.rb")
                ("app/helpers/\\1.rb"     . "spec/helpers/\\1_spec.rb")))
    (rails   . (("app/controllers/\\1.rb" . "test/functional/\\1_test.rb")
                ("app/models/\\1.rb"      . "test/unit/\\1_test.rb")
                ("lib/\\1.rb"             . "test/unit/test_\\1.rb")))
    (ruby    . (("lib/\\1.rb"             . "test/test_\\1.rb")
                ("\\1.rb"                 . "test_\\1.rb")))
    (elixir  . (("lib/\\1.ex"             . "test/test_\\1.exs")
                ("\\1.ex"                 . "test_\\1.exs"))))
  "A list of (name . quick-toggle-mapping) rules used by quick-toggle-filename."
  :group 'quick-toggle
  :type '(repeat (cons string string)))

(defcustom quick-toggle-mapping-style
  'ruby
  "The default quick-toggle mapping style to load when initialized."
  :group 'quick-toggle
  :type '(symbol))

(defvar quick-toggle-mappings ()
  "*The current file mappings for `quick-toggle-filename' to use.")

(defun quick-toggle-style (name)
  (interactive (list (completing-read "Style: "
                                      (mapcar
                                       #'symbol-name
                                       (mapcar #'car quick-toggle-mapping-styles))
                                      nil t "")))
  (let* ((style (if (stringp name) (intern name) name))
         (pairs (cdr (assoc style quick-toggle-mapping-styles))))
    (if pairs
        (let ((mappings
               (mapcar (lambda (pair)
                         (cons
                          (replace-regexp-in-string
                           "\\\\1" "\\\\(.*\\\\)"
                           (replace-regexp-in-string ; special case for "\\1.ext"
                            "^\\\\1" "\\\\([^/]*\\\\)" (car pair)))
                          (cdr pair)))
                       (cl-mapcan 'list
                               pairs
                               (mapcar (lambda (pair)
                                         (cons (cdr pair) (car pair)))
                                       pairs)))))
          (if (called-interactively-p 'interactive)
              (setq quick-toggle-mappings mappings)
            mappings))
      nil)))

(defun quick-toggle-filename (path rules)
  "Transform a matching subpath in PATH as given by RULES.
Each element in RULES is a pair (RE . TRANS). If the regular
expression RE matches PATH, then replace-match is invoked with
TRANS. After the first successful match, this returns. If no rule
matches, it returns nil"
  (cond ((null rules) nil)
    ((string-match (caar rules) path)
     (replace-match (cdar rules) nil nil path))
    (t (quick-toggle-filename path (cl-rest rules)))))

(defun quick-toggle-buffer ()
  "Opens a related file to the current buffer using matching rules.
Matches the current buffer against rules in quick-toggle-mappings. If a
match is found, switches to that buffer."
  (interactive)
  (let ((new-name (quick-toggle-filename (buffer-file-name) quick-toggle-mappings)))
    (if new-name
        (find-file new-name)
      (message (concat "Match not found for " (buffer-file-name))))))

(defun quick-toggle-find-matcher (map target)
  (cl-loop
   for matcher
   being the hash-keys
   in map
   if (string-match-p matcher target)
   return matcher))

(defun quick-toggle-apply-matcher (matcher target)
  "/home/niku/projects/foo/spec/lib/foo_spec.rb")

(provide 'quick-toggle)
;;; quick-toggle.el ends here
