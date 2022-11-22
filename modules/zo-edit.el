;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(use-package smartparens
  :config
  (general-def '(normal visual)
    "gb" 'sp-splice-sexp)
  :hook (prog-mode . smartparens-mode))
(use-package link-hint
  :ensure t
  :defer 2
  :config
  (general-def 'normal Info-mode-map
    "o"       #'link-hint-open-link)
  (general-def 'normal help-mode-map
    "o"       #'link-hint-open-link)
  )

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" js-mode-syntax-table)
(modify-syntax-entry ?_ "w" js2-mode-syntax-table)

(advice-add 'evil-ex-search-next :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))

(provide 'zo-edit)
