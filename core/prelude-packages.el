;;; prelude-packages.el --- Emacs Prelude: default package selection.
;;; Commentary:
;;; License:
;;; Code:

(require 'cl-lib)
(require 'package)
;;;; Package setup and additional utility functions

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; load the pinned packages
(let ((prelude-pinned-packages-file (expand-file-name "prelude-pinned-packages.el" prelude-dir)))
  (if (file-exists-p prelude-pinned-packages-file)
      (load prelude-pinned-packages-file)))

;; set package-user-dir to be relative to Prelude install path
(setq package-user-dir (expand-file-name "elpa" prelude-dir))
(package-initialize)

(defvar prelude-packages
  '(use-package
    ace-window
    ag
    avy
    anzu
    browse-kill-ring
    crux
    discover-my-major
    diff-hl
    diminish
    easy-kill
    editorconfig
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    git-modes
    hl-todo
    imenu-anywhere
    projectile
    magit
    move-text
    nlinum
    operate-on-number
    smartparens
    smartrep
    super-save
    volatile-highlights
    which-key
    zenburn-theme
    zop-to-char
    undo-fu ;; must come before evil
    link-hint
    general ripgrep pcre2el better-jumper restart-emacs dumb-jump request
    )
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (cl-every #'package-installed-p prelude-packages))

(defun prelude-require-package (package)

  "Install PACKAGE unless already installed."
  (unless (memq package prelude-packages)
    (add-to-list 'prelude-packages package))
  (unless (package-installed-p package)
    (package-install package))
  ;; (eval-when-compile
  ;;   (unless (bound-and-true-p package--initialized)
  ;;     (package-initialize))  ;; be sure load-path includes package directories
  ;;   (require 'use-package)
  ;;   )
  )

(defun prelude-require-packages (packages)
  "Ensure PACKAGES are installed.
Missing packages are installed automatically."
  (mapc #'prelude-require-package packages))

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (prelude-require-packages prelude-packages)))

;; run package installation
(prelude-install-packages)

;load it using require
(require 'use-package)
;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)

(defun prelude-list-foreign-packages ()
  "Browse third-party packages not bundled with Prelude.

Behaves similarly to `package-list-packages', but shows only the packages that
are installed and are not in `prelude-packages'.  Useful for
removing unwanted packages."
  (interactive)
  (package-show-package-list
   (cl-set-difference package-activated-list prelude-packages)))

;;;; Auto-installation of major modes on demand

(defmacro prelude-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar prelude-auto-install-alist
  '(("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown-mode doesn't have autoloads for the auto-mode-alist
;; so we add them manually if it's already installed
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; and pkgbuild-mode
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; build auto-install mappings
(mapc
 (lambda (entry)
   (let ((extension (car entry))
         (package (cadr entry))
         (mode (cadr (cdr entry))))
     (unless (package-installed-p package)
       (prelude-auto-install extension package mode))))
 prelude-auto-install-alist)

(provide 'prelude-packages)
;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; prelude-packages.el ends here
