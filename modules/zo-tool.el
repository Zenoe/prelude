;; (defvar init-base-dir)
(defvar auto-gen-file (expand-file-name "autoload/autoload-gen.el" init-base-dir))
(defun zo/update-all-autoloads ()
  (interactive)
  (cd init-base-dir)
  (make-directory-autoloads
   '("lib" "modules/workspaces/autoload" "modules/lookup/autoload")
   auto-gen-file)
  )

(when (not (file-exists-p auto-gen-file ))
    (zo/update-all-autoloads)
  )

(zo/load  "autoload/autoload-gen" init-base-dir)
(defun zo/delete-window-by-name (&optional name)
  "Delete popup window which got the name of *abc* form."
  (let (target-window)
    (setq target-window
          (car (seq-filter
                (lambda (window)
                  (if name
                      (equal name (buffer-name (window-buffer window)))
                    ( string-match-p "^\\*.+\\*$" (buffer-name (window-buffer window)))
                    )
                  )
                (window-list-1 nil 0 t))))
    (when target-window
      (delete-window target-window)
      )
    )
  )


(advice-add 'keyboard-quit :before
            #'zo/delete-window-by-name
            )
(advice-add 'evil-force-normal-state :before
            #'zo/delete-window-by-name
            )
;; (zo/delete-window-by-name "*Help*")
;; (zo/delete-window-by-name )

;; (defun set-proxy()
;;   (interactive)
;;   (setq url-proxy-services
;;         '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
;;           ("http" . "127.0.0.1:7890")
;;           ("https" . "127.0.0.1:7890")))
;;   )
