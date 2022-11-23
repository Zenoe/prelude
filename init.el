;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2022 Bozhidar Batsov
;;; Commentary:
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
                                        ;(package-initialize)
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defvar prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "[Prelude] Prelude is powering up... Be patient, Master %s!" prelude-user)

(set-default-coding-systems 'utf-8)

(when (version< emacs-version "25.1")
  (error "[Prelude] Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define Prelude's directory structure
(defvar prelude-dir (file-name-directory load-file-name))

(defvar init-base-dir (file-name-directory load-file-name))

(defvar prelude-core-dir (expand-file-name "core" init-base-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" init-base-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" init-base-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-personal-dir)
  "This file contains a list of modules that will be loaded by Prelude.")

(defvar autoload-dir (expand-file-name "autoload" init-base-dir))


(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

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

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Prelude] Loading Prelude's core modules...")

;; load the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'zo-packages)

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

(message "[Prelude] Loading Prelude's additional modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "[Prelude] Missing personal modules file %s" prelude-modules-file)
  (message "[Prelude] Falling back to the bundled example file sample/prelude-modules.el")
  (message "[Prelude] You should copy this file to your personal configuration folder and tweak it to your liking")
  (load (expand-file-name "sample/prelude-modules.el" init-base-dir)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))


(defvar custom-dir (expand-file-name "custom" init-base-dir))
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))
;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (delete
               prelude-modules-file
               (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Prelude] Prelude is ready to do thy bidding, Master %s!" prelude-user)

;; (prelude-eval-after-init
;;  (run-at-time 5 nil 'prelude-tip-of-the-day))

;;; init.el ends here
