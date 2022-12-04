;; https://company-mode.github.io/manual/Backends.html#Backends
(use-package company
  :commands (company-complete-common
             company-complete-common-or-cycle
             company-manual-begin
             company-grab-line)

  :init
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-eclim-auto-save nil          ; Stop eclim auto save.
        company-dabbrev-downcase nil
        company-require-match 'never
        company-global-modes
        '(not erc-mode
              circe-mode
              message-mode
              help-mode
              gud-mode
              vterm-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area

        ;; Buffer-local backends will be computed when loading a major mode, so
        ;; only specify a global default here.
        company-backends
        '((company-files          ; files & directory
           company-keywords       ; keywords
           company-dabbrev        ; put buffer source before capf with the help of 'separate'
           company-capf
           company-yasnippet
           :separate
           )
          ;; (company-abbrev company-dabbrev)
          )
        ;; These auto-complete the current selection when
        ;; `company-auto-commit-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-commit nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-files-exclusions '(".git/" ".DS_Store")

        company-transformers '(delete-consecutive-dups
                               company-sort-prefer-same-case-prefix
                               company-sort-by-occurrence)
        )

  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           'company-anaconda)))
  (dolist (hook '(js-mode-hook
                  js2-mode-hook
                  js3-mode-hook
                  inferior-js-mode-hook
                  ))
    (add-hook hook
              (lambda ()
                (tern-mode t)

                (add-to-list (make-local-variable 'company-backends)
                             'company-tern)
                )))


;;;_. company-mode support like auto-complete in web-mode

  ;; Enable CSS completion between <style>...</style>
  (defadvice company-css (before web-mode-set-up-ac-sources activate)
    "Set CSS completion based on current language before running `company-css'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (string= web-mode-cur-language "css")
              (unless css-mode (css-mode))))))

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running `company-tern'."
    (if (equal major-mode 'web-mode)
        (let ((web-mode-cur-language (web-mode-language-at-pos)))
          (if (or (string= web-mode-cur-language "javascript")
                  (string= web-mode-cur-language "jsx"))
              (unless tern-mode (tern-mode))
            ;; (if tern-mode (tern-mode))
            ))))
  )

(use-package company-fuzzy
  :hook (company-mode . company-fuzzy-mode)
  :init
  (setq company-fuzzy-sorting-backend 'alphabetic
        company-fuzzy-prefix-on-top t
        company-fuzzy-trigger-symbols '("." "->" "<" "\"" "'" "@")))
