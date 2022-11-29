;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2022 Bozhidar Batsov
;;; Commentary:
;;; Code:

(load ( expand-file-name  "default-config"  (file-name-directory load-file-name) ))

(defun prelude-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path autoload-dir)
(add-to-list 'load-path (expand-file-name "search" init-base-dir))

(add-to-list 'load-path (expand-file-name "lisp" init-base-dir))
;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

;; load the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)

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
  (require 'prelude-windows))

;; the modules
(load (expand-file-name "load-modules.el" init-base-dir))
;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))


(defvar custom-dir (expand-file-name "custom" init-base-dir))
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$")))


(run-hooks 'zo-after-init-hook)
(message "[Prelude] Prelude is ready to do thy bidding" )

;; (prelude-eval-after-init
;;  (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
