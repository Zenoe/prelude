;; -*- lexical-binding: t; -*-
;; Startup time
(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(defvar last-file-name-handler-alist file-name-handler-alist)
;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
;; (setq gc-cons-threshold 50000000)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(defun zo/load (modules? parent-dir)
  (if (listp modules?)
      (dolist (mod modules?)
        ;; set third param t to avoid loading message
        (load (expand-file-name mod parent-dir) nil t))
    (load (expand-file-name modules? parent-dir) nil t)
    )
  )

(zo/load "default-config" (file-name-directory load-file-name))

;; need this. for autoload files to be found
;; if not complains about cant find ../lib/file jump out
(add-to-list 'load-path (expand-file-name "xxx" init-base-dir))

(let ((zo-modules '(
                     "prelude-packages"
                     "prelude-custom"
                     "prelude-core"
                     "prelude-mode"
                     )))
  (zo/load zo-modules prelude-core-dir)
  )

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))
;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'prelude-linux))
;; WSL specific setting
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (require 'prelude-wsl))
;; Windows specific settings
(when (eq system-type 'windows-nt)
  (zo/load "prelude-windows" prelude-core-dir)
  )

;; the modules

(zo/load "load-modules" init-base-dir)
;; config changes made through the customize UI will be stored here

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(run-hooks 'zo-after-init-hook)

(message "ready to do thy bidding" )

(run-with-idle-timer
 4 nil
 (lambda ()
   (setq gc-cons-threshold 16777216
         gc-cons-percentage 0.1
         file-name-handler-alist last-file-name-handler-alist)
   ))

;; after startup, it is important you reset this to some reasonable default. A large
;; gc-cons-threshold will cause freezing and stuttering during long-term
;; interactive use. I find these are nice defaults:

;; (add-hook! 'emacs-startup-hook
;;   )
;; ;; (prelude-eval-after-init
;;  (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
