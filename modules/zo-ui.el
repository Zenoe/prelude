(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)
;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(if (fboundp 'global-display-line-numbers-mode)
      (global-display-line-numbers-mode)
    (global-nlinum-mode t))

(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Prelude - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))

;; use zenburn as the default theme
(when prelude-theme
  (load-theme prelude-theme t))

;; show available keybindings after you start typing
;; add to hook when running as a daemon as a workaround for a
;; which-key bug
;; https://github.com/justbur/emacs-which-key/issues/306
(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
  (which-key-mode +1))


(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)
;; Don't warn for large files (shows up when launching videos)
;; (setq large-file-warning-threshold nil)
(setq help-window-select t)
