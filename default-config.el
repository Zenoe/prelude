;; config of emacs itself. no 3rd party package config

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; vars
(defvar zo-after-init nil
  "Transient hooks run before the first user input.")
(put 'zo-after-init 'permanent-local t)

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
      (expand-file-name "emacs-local/data" (getenv-internal "APPDATA"))
    (expand-file-name "emacs-local/data" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share/")))
  "stores global data files here
Use this for: server binaries, package source, pulled module libraries,
generated files for profiles, profiles themselves, autoloads/loaddefs, etc.
For profile-local data files, use `doom-profile-data-dir' instead.")

(defvar emacs-cache-dir
  (if IS-WINDOWS
      (expand-file-name "emacs-local/cache" (getenv-internal "APPDATA"))
    (expand-file-name "emacs-local/cache" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share/")))
  )
(defvar zo-local-dir
  (if IS-WINDOWS
      (expand-file-name "emacs-local/zo-local" (getenv-internal "APPDATA"))
    (expand-file-name "emacs-local/zo-local" (or (getenv-internal "XDG_DATA_HOME") "~/.local/share/")))
  "system specified data dir can not be shared across systems"
  )
(defvar autoload-dir (expand-file-name "autoload" init-base-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs settings
(set-default-coding-systems 'utf-8)
(menu-bar-mode -1)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Always load newest byte code
(setq load-prefer-newer t)
(when (version< emacs-version "25.1")
  (error "Requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))
(make-directory emacs-data-dir 'parents)
(setq user-emacs-directory emacs-data-dir
      url-history-file (expand-file-name "url/history" user-emacs-directory))
(setq dired-clean-confirm-killing-deleted-buffers nil)

;; Auto-refresh dired on file change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Don't warn for large files (shows up when launching videos)
(setq large-file-warning-threshold nil)
;; Don't warn for following symlinked files
(setq vc-follow-symlinks t)
;; Don't warn when advice is added for functions
(setq ad-redefinition-action 'accept)

;; Set default connection mode to SSH
(setq tramp-default-method "ssh")


(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      ;; backup-directory-alist (list (cons "." (concat doom-cache-dir "backup/")))
      )

;; Create missing directories when we open a file that doesn't exist under a
;; directory tree that may not exist.
(add-hook 'find-file-not-found-functions
  (defun doom-create-missing-directories-h ()
    "Automatically create missing directories when creating new files."
    (unless (file-remote-p buffer-file-name)
      (let ((parent-directory (file-name-directory buffer-file-name)))
        (and (not (file-directory-p parent-directory))
             (y-or-n-p (format "Directory `%s' does not exist! Create it?"
                               parent-directory))
             (progn (make-directory parent-directory 'parents)
                    t))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hidden-minor-modes ; example, write your own list of hidden
  '(smartparens-mode
    which-key-mode
    evil-collection-unimpaired-mode
    persp-mode
    whitespace-mode
    prelude-mode
    abbrev-mode
    auto-fill-function
    flyspell-mode
    projectile-mode
    smooth-scroll-mode))
(defun purge-minor-modes ()
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
;; remove all mino-mode from mode-line
;; (setq mode-line-modes
;;       (mapcar (lambda (elem)
;;                 (pcase elem
;;                   (`(:propertize (,_ minor-mode-alist . ,_) . ,_)
;;                    "")
;;                   (t elem)))
;;               mode-line-modes))
;; (message (expand-file-name "configs/other-config" (file-name-directory (buffer-file-name))))

;; Translate the problematic keys to the function key Hyper:
(keyboard-translate ?\C-m ?\H-m)
;; Rebind then accordantly:
(global-set-key [?\H-m] 'delete-backward-char)
(keyboard-translate ?\C-i ?\H-i)
(global-set-key [?\H-i] 'better-jumper-jump-forward)
(load (expand-file-name "configs/other-config" init-base-dir))
