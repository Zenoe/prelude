(use-package persp-mode
  ;; :bind
  ;; :custom
  ;; (persp-mode-prefix-key (kbd "C-c k"))  ; pick your own prefix key here
  ;; (unless persp-mode (persp-mode +1))
  ;; ;; (setq persp-auto-save-opt 0)
  ;; ;; ??Running `persp-mode' multiple times resets the perspective list...
  ;; ;; (unless (equal persp-mode t)
  ;; ;;   (persp-mode))
  )

    (with-eval-after-load "persp-mode"
      (setq wg-morph-on nil)
      (setq persp-autokill-buffer-on-remove 'kill-weak)
      (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
    (require 'persp-mode)


(provide 'zo-persp-mode)
