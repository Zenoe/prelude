;;; prelude-evil.el --- Emacs Prelude: evil-mode configuration.
;;; Code:

;;; goto-chg lets you use the g-; and g-, to go to recent changes
;;; evil-visualstar enables searching visual selection with *
;;; evil-numbers enables vim style numeric incrementing and decrementing

(global-set-key (kbd "C-M-u") 'universal-argument)

;; (prelude-require-packages '(goto-chg evil-surround evil-visualstar evil-numbers))

;; (setq evil-mode-line-format 'before)

;; (setq evil-emacs-state-cursor  '("red" box))
;; (setq evil-normal-state-cursor '("gray" box))
;; (setq evil-visual-state-cursor '("gray" box))
;; (setq evil-insert-state-cursor '("gray" bar))
;; (setq evil-motion-state-cursor '("gray" box))

;; ;; prevent esc-key from translating to meta-key in terminal mode
;; (global-evil-surround-mode 1)

;; (define-key evil-normal-state-map (kbd "C-A")
;;   'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-S-A")
;;   'evil-numbers/dec-at-pt)

;;
;; Other useful Commands
;;
;; (evil-ex-define-cmd "W"     'evil-write-all)
;; (evil-ex-define-cmd "Tree"  'speedbar-get-focus)
;; (evil-ex-define-cmd "Align" 'align-regexp)

;;
;; Magit from avsej
;;
;; (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
;; (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
;; (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
;;   "K" 'magit-discard
;;   "L" 'magit-log)
;; (evil-add-hjkl-bindings magit-status-mode-map 'emacs
;;   "K" 'magit-discard
;;   "l" 'magit-log
;;   "h" 'magit-diff-toggle-refine-hunk)


;;; snagged from Eric S. Fraga
;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-05/msg00153.html

(defun current-line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

(with-eval-after-load 'evil
  (defun prelude-shift-left-visual ()
    "Shift left and restore visual selection."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun prelude-shift-right-visual ()
    "Shift right and restore visual selection."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun prelude-evil-key-bindings-for-org ()
    ;;(message "Defining evil key bindings for org")
    (evil-declare-key 'normal org-mode-map
      "gk" 'outline-up-heading
      "gj" 'outline-next-visible-heading
      "H" 'org-beginning-of-line ; smarter behaviour on headlines etc.
      "L" 'org-end-of-line ; smarter behaviour on headlines etc.
      "t" 'org-todo ; mark a TODO item as DONE
      ",c" 'org-cycle
      (kbd "TAB") 'org-cycle
      ",e" 'org-export-dispatch
      ",n" 'outline-next-visible-heading
      ",p" 'outline-previous-visible-heading
      ",t" 'org-set-tags-command
      ",u" 'outline-up-heading
      "$" 'org-end-of-line ; smarter behaviour on headlines etc.
      "^" 'org-beginning-of-line ; ditto
      "-" 'org-ctrl-c-minus ; change bullet style
      "<" 'org-metaleft ; out-dent
      ">" 'org-metaright ; indent
      ))

  (prelude-evil-key-bindings-for-org)
  )

(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

(defun force-normal-n-save ()
  (interactive)
  (evil-force-normal-state)
  (save-buffer)
  )

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (unless ( current-line-empty-p )
    (evil-open-below 1)
    )
  (yank)
  (evil-force-normal-state)
  (call-interactively 'indent-region))

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-esc-delay 0)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "zx") 'kill-current-buffer)
  (define-key evil-normal-state-map (kbd "zp") 'yank-and-indent)
  (define-key evil-insert-state-map (kbd "M-;") 'yank)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (setq evil-shift-width 2)

  ;;; enable avy with evil-mode
  (define-key evil-normal-state-map (kbd "SPC gw") 'avy-goto-word-1)
  (define-key evil-visual-state-map (kbd ">") 'prelude-shift-right-visual)
  (define-key evil-visual-state-map (kbd "<") 'prelude-shift-left-visual)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode)
  )

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode t)
  )

(provide 'zo-evil)
