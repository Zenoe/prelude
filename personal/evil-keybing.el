;; set leader key in normal state
(require 'general)
(require 'evil)
(require 'autoload-gen)

(global-set-key (kbd "C-s") 'force-normal-n-save)
(defun doom/escape (&optional interactive)
  "Run `doom-escape-hook'."
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'doom-escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))

(global-set-key [remap keyboard-quit] #'doom/escape)
(define-key evil-normal-state-map [escape] #'doom/escape)

(defvar doom-leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> keys.")

(define-prefix-command 'doom/leader 'doom-leader-map)
(define-key doom-leader-map [override-state] 'all)
(defvar doom-leader-key "SPC"
  "The leader prefix key for Evil users.")
(defvar doom-leader-alt-key "M-SPC"
  "An alternative leader prefix key, used for Insert and Emacs states, and for
non-evil users.")

;; Bind `doom-leader-key' and `doom-leader-alt-key' as late as possible to give
;; the user a chance to modify them.
(defun doom-init-leader-keys-h ()
    "Bind `doom-leader-key' and `doom-leader-alt-key'."
    (let ((map general-override-mode-map))
      (if (not (featurep 'evil))
          (progn
            (cond ((equal doom-leader-alt-key "C-c")
                   (set-keymap-parent doom-leader-map mode-specific-map))
                  ((equal doom-leader-alt-key "C-x")
                   (set-keymap-parent doom-leader-map ctl-x-map)))
            (define-key map (kbd doom-leader-alt-key) 'doom/leader))
        (evil-define-key* '(normal visual motion) map (kbd doom-leader-key) 'doom/leader)
        (evil-define-key* '(emacs insert) map (kbd doom-leader-alt-key) 'doom/leader))
      (general-override-mode +1)))

(add-hook 'after-init-hook #'doom-init-leader-keys-h)
;; (evil-set-leader 'normal (kbd "SPC"))
;; (defvar my-leader-map (make-sparse-keymap)
;;   "Keymap for \"leader key\" shortcuts.")
;; (define-key evil-normal-state-map "," 'evil-repeat-find-char-reverse)
;; (define-key evil-normal-state-map (kbd "SPC") my-leader-map)

;; (define-key doom-leader-map "b" 'list-buffers)

(evil-define-key nil doom-leader-map
  " " 'projectile-find-file
  "fr" 'consult-recent-file
  "fD" 'doom/delete-this-file
  "fR" 'doom/move-this-file
  "bb" 'projectile-switch-to-buffer
  "bz" 'bury-buffer
  "." 'find-file
  "ha" 'consult-apropos
  ";" 'evil-switch-to-windows-last-buffer
  "/" '+default/search-project
  "*" '+default/search-project-for-symbol-at-point
  "'" 'vertico-repeat
  "pi" ' projectile-invalidate-cache
)

(general-create-definer my-local-leader-def
  ;; :prefix my-local-leader
  :prefix "SPC TAB")

(my-local-leader-def '(normal visual)
  "TAB" #'+workspace/display
  "."   #'+workspace/switch-to
  "`"   #'+workspace/other
  "n"   #'+workspace/new
  "N"   #'+workspace/new-named
  "l"   #'+workspace/load
  "s"   #'+workspace/save
  "x"   #'+workspace/kill-session
  "d"   #'+workspace/delete
  "r"   #'+workspace/rename
  "R"   #'+workspace/restore-last-session
  "]"   #'+workspace/switch-right
  "["   #'+workspace/switch-left
  ;; ...
  )
(general-def
  "M-1"   #'+workspace/switch-to-0
  "M-2"   #'+workspace/switch-to-1
  "M-3"   #'+workspace/switch-to-2
  "M-4"   #'+workspace/switch-to-3
  "M-5"   #'+workspace/switch-to-4
  "M-6"   #'+workspace/switch-to-5
  "M-7"   #'+workspace/switch-to-6
  "M-8"   #'+workspace/switch-to-7
  "M-9"   #'+workspace/switch-to-8
  "M-0"   #'+workspace/switch-to-final
  )
(global-set-key (kbd "C-x p") 'proced)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
