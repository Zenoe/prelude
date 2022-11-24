(require 'prelude-company)
;; (require 'prelude-key-chord) ;; Binds useful features to key combinations
(require 'prelude-org)
(require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
;; (require 'prelude-dart)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erlang)
;; (require 'prelude-go)
(require 'prelude-js)
;; (require 'prelude-latex)
(require 'prelude-lisp) ;; Common setup for Lisp-like languages
;; (require 'prelude-literate-programming) ;; Setup for Literate Programming
(require 'prelude-lsp) ;; Base setup for the Language Server Protocol
;; (require 'prelude-lua)
(require 'prelude-perl)
;; (require 'prelude-racket)
;; (require 'prelude-rust)
;; (require 'prelude-scala)
(require 'prelude-shell)
;; (require 'prelude-scss)
;; (require 'prelude-ts)
(require 'prelude-web) ;; Emacs mode for web templates
(require 'prelude-xml)
(require 'prelude-yaml)
;;; Misc
(require 'prelude-erc) ;; A popular Emacs IRC client (useful if you're still into Freenode)

(require 'zo)
(require 'zo-tool)
(require 'zo-vertico) ;; A mighty modern alternative to ido
(require 'zo-evil)
(require 'zo-persp-mode)

(load (expand-file-name "workspaces/config" prelude-modules-dir))
(provide 'load-modules)
;;; load-modules.el ends here
