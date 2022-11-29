;; (add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(use-package smartparens
  :commands sp-pair sp-local-pair sp-with-modes sp-point-in-comment sp-point-in-string
  :hook (prog-mode . smartparens-mode)
  :config
  (general-def '(normal visual)
    "gb" 'sp-splice-sexp)
  ;; Autopair quotes more conservatively; if I'm next to a word/before another
  ;; quote, I don't want to open a new pair or it would unbalance them.
  (with-eval-after-load 'smartparens
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

(use-package better-jumper
  :hook (zo-after-init . better-jumper-mode)
  :commands doom-set-jump-a doom-set-jump-maybe-a doom-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward)
  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun doom-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn actually moves the point."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args)))
          (dest (point-marker)))
      (unless (equal origin dest)
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      (set-marker origin nil)
      (set-marker dest nil)
      result))

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w" js-mode-syntax-table)
(modify-syntax-entry ?_ "w" js2-mode-syntax-table)

(advice-add 'evil-ex-search-next :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))
(advice-add 'evil-ex-search-previous :after
            (lambda (&rest x) (evil-scroll-line-to-center (line-number-at-pos))))


(provide 'zo-edit)
