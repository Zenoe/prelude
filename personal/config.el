(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;; Reduce unnecessary/unactionable warnings/logs
;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(if (not window-system)
    (use-package xclip
      :config
      (xclip-mode 1)
      )
  )


(require 'smartparens)
;; (smartparens-strict-mode -1)
(smartparens-global-strict-mode -1)

;; delete file without prompts
;; (advice-add 'dired-delete-file :before
;;             (lambda (file &rest rest)
;;               (when-let ((buf (get-file-buffer file)))
;;                 (kill-buffer buf))))

;; (define-key global-map [remap list-buffers] 'buffer-menu-other-window)
;; (add-to-list 'savehist-additional-variables 'extended-command-history)
;; (add-to-list 'savehist-additional-variables 'command-history)
