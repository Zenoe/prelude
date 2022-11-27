(require 'general)
(require 'evil)

(global-set-key (kbd "C-M-u") 'universal-argument)
(global-set-key (kbd "C-s") 'force-normal-n-save)

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

(evil-define-key nil doom-leader-map
  " " 'projectile-find-file
  "." 'find-file
  " " 'projectile-find-file
  ";" 'evil-switch-to-windows-last-buffer
  "/" '+default/search-project
  "*" '+default/search-project-for-symbol-at-point
  "'" 'vertico-repeat
  "fr" 'consult-recent-file
  "fD" 'doom/delete-this-file
  "fR" 'doom/move-this-file
  "bb" 'projectile-switch-to-buffer
  "bz" 'bury-buffer
  "bB" 'consult-buffer
  "ha" 'consult-apropos
  "qq" 'restart-emacs
  "tw" 'whitespace-mode
  "tt" '(counsel-load-theme :which-key "choose theme")
  "pi" 'projectile-invalidate-cache
  "uu" 'universal-argument
  )

(general-create-definer workspace-leader
  :prefix "SPC TAB"
  ;; :non-normal-prefix ""
  )

(workspace-leader '(normal visual)
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
