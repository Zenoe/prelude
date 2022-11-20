(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;; Reduce unnecessary/unactionable warnings/logs
;; Disable warnings from the legacy advice API. They aren't actionable or
;; useful, and often come from third party packages.
(setq ad-redefinition-action 'accept)

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(defvaralias)))
(setq comp-async-report-warnings-errors nil)
(require 'xclip)
(xclip-mode 1)

(menu-bar-mode -1)

(global-undo-tree-mode)

(require 'evil)
(evil-set-undo-system 'undo-tree)

(require 'smartparens)
;; (smartparens-strict-mode -1)
(smartparens-global-strict-mode -1)

;; delete file without prompts
;; (advice-add 'dired-delete-file :before
;;             (lambda (file &rest rest)
;;               (when-let ((buf (get-file-buffer file)))
;;                 (kill-buffer buf))))

;; delete without prompting for kill buffer
;; (defadvice dired-delete-entry (before force-clean-up-buffers (file) activate)
;;   (kill-buffer (get-file-buffer file)))
(setq dired-clean-confirm-killing-deleted-buffers nil)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(setq help-window-select t)
;; (define-key global-map [remap list-buffers] 'buffer-menu-other-window)

(add-to-list 'savehist-additional-variables 'extended-command-history)
(add-to-list 'savehist-additional-variables 'command-history)
