;; config of emacs itself. no 3rd party package config

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vars
(defvar prelude-dir (file-name-directory load-file-name))
(defvar init-base-dir (file-name-directory load-file-name))
(defvar prelude-core-dir (expand-file-name "core" init-base-dir))
(defvar prelude-modules-dir (expand-file-name  "modules" init-base-dir))
(defvar prelude-personal-dir (expand-file-name "personal" init-base-dir))
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration, that you want loaded before Prelude.")
(defvar prelude-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))
(defvar emacs-data-dir
  (if IS-WINDOWS
      (expand-file-name "emacs-local/" (getenv-internal "APPDATA"))
    (expand-file-name "emacs-local/" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share")))
  "Where Doom stores its global data files.

Data files contain shared and long-lived data that Doom, Emacs, and their
packages require to function correctly or at all. Deleting them by hand will
cause breakage, and require user intervention (e.g. a 'doom sync' or 'doom env')
to restore.
Use this for: server binaries, package source, pulled module libraries,
generated files for profiles, profiles themselves, autoloads/loaddefs, etc.
For profile-local data files, use `doom-profile-data-dir' instead.")
(message emacs-data-dir)
(defvar autoload-dir (expand-file-name "autoload" init-base-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs settings
(set-default-coding-systems 'utf-8)
(menu-bar-mode -1)
;; Always load newest byte code
(setq load-prefer-newer t)
(when (version< emacs-version "25.1")
  (error "Requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))
(make-directory emacs-data-dir 'parents)
(setq user-emacs-directory (expand-file-name "emacs-local" (getenv-internal "APPDATA"))
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq dired-clean-confirm-killing-deleted-buffers nil)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

(setq ad-redefinition-action 'accept)
(setq help-window-select t)

;; Ignore warnings about "existing variables being aliased". Otherwise the user
;; gets very intrusive popup warnings about our (intentional) uses of
;; defvaralias, which are done because ensuring aliases are created before
;; packages are loaded is an unneeded and unhelpful maintenance burden. Emacs
;; still aliases them fine regardless.
(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-types '(defvaralias)))
(setq comp-async-report-warnings-errors nil)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'default-config)
