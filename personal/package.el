(require 'util)
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
     ("http" . "127.0.0.1:7890")
     ("https" . "127.0.0.1:7890")))

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")

;; (prelude-require-packages '(xclip general embark orderless marginalia))
