;; misc config
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (flycheck-disable-checker 'emacs-lisp-checkdoc)
;;             ))

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
