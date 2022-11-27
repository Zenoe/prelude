;; misc config
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flycheck-disable-checker 'emacs-lisp-checkdoc)
            ))
