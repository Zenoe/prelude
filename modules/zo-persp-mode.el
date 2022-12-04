(use-package persp-mode
  )

(with-eval-after-load 'persp-mode
  (add-hook 'window-setup-hook #'(lambda () (persp-mode 1))))
