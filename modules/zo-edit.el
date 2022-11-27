;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(use-package smartparens
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :hook (prog-mode . smartparens-mode)
  :config
  (general-def '(normal visual)
    "gb" 'sp-splice-sexp)
  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.


  )

(with-eval-after-load 'smartparens-mode
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  )

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
