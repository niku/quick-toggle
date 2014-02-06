(require 'f)
(require 'el-mock)

(defvar quick-toggle/test-path
  (f-dirname (f-this-file)))

(defvar quick-toggle/root-path
  (f-parent quick-toggle/test-path))

(load (f-expand "quick-toggle" quick-toggle/root-path))

(add-to-list 'load-path quick-toggle/root-path)
