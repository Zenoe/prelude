;;; tools/lookup/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?" This module helps you answer this question.
;;
;;   + `+lookup/definition': a jump-to-definition that should 'just work'
;;   + `+lookup/implementations': find a symbol's implementations in the current
;;                                project
;;   + `+lookup/references': find a symbol's references in the current project
;;   + `+lookup/file': open the file referenced at point
;;   + `+lookup/online'; look up a symbol on online resources
;;   + `+lookup/in-docsets': look up in Dash docsets
;;
;; This module uses `xref', an experimental new library in Emacs. It may change
;; in the future. When xref can't be depended on it will fall back to
;; `dumb-jump' to find what you want.

(defvar +lookup-provider-url-alist
  (append '(("Doom Emacs issues" "https://github.com/hlissner/doom-emacs/issues?q=is%%3Aissue+%s")
            ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
            ("Google images"     "https://www.google.com/images?q=%s")
            ("Google maps"       "https://maps.google.com/maps?q=%s")
            ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s"))
          ;; (when (modulep! :lang rust)
          ;;   '(("Rust Docs" "https://doc.rust-lang.org/std/?search=%s")))
          )
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `+lookup/online'.")

(defvar +lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar +lookup-definition-functions
  '(+lookup-dictionary-definition-backend-fn
    +lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-implementations-functions ()
  "Function for `+lookup/implementations' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-type-definition-functions ()
  "Functions for `+lookup/type-definition' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-thesaurus-definition-backend-fn
    +lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")


;;
;;; dumb-jump

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project user-emacs-directory
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-selector
        (cond ((fboundp 'ivy-mode)  'ivy)
              ('popup)))
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))


;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(after! xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defadvice! +lookup--projectile-find-tag-a (fn)
    :around #'projectile-find-tag
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall fn)))

  ;; This integration is already built into evil
  (unless (fboundp 'evil-mode)
    ;; Use `better-jumper' instead of xref's marker stack
    (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a))
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; (use-package consult-xref
  ;;   :defer t
  ;;   :init
  ;;   (setq xref-show-xrefs-function       #'consult-xref
  ;;         xref-show-definitions-function #'consult-xref))
  )


;;
;;; Dash docset integration

;; (use-package dash-docs
;;   :when (modulep! +docsets)
;;   :defer t
;;   :init
;;   (add-hook '+lookup-documentation-functions #'+lookup-dash-docsets-backend-fn)
;;   :config
;;   (setq dash-docs-enable-debugging init-file-debug
;;         dash-docs-docsets-path (concat doom-data-dir "docsets/")
;;         dash-docs-min-length 2
;;         dash-docs-browser-func #'eww)

;;   (cond ((modulep! :completion helm)
;;          (require 'helm-dash nil t))
;;         ((modulep! :completion ivy)
;;          (require 'counsel-dash nil t))))


;;
;;; Dictionary integration

;; (use-package define-word
;;   :when (modulep! +dictionary)
;;   :unless IS-MAC
;;   :defer t
;;   :config
;;   (setq define-word-displayfn-alist
;;         (cl-loop for (service . _) in define-word-services
;;                  collect (cons service #'+eval-display-results-in-popup))))


;;;###package synosaurus
(setq synosaurus-choose-method 'default) ; use ivy/helm instead of ido
