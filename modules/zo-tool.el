(defun zo/update-all-autoloads ()
  (interactive)
  (cd init-base-dir)
  (setq generated-autoload-file (expand-file-name "autoload/autoload-gen.el"))
  (make-directory-autoloads
   '("lisp/lib" "search" "modules/workspaces/autoload" "modules/lookup/autoload")
   generated-autoload-file)
  )
(if (not generated-autoload-file )
    (zo/update-all-autoloads))
(require 'autoload-gen)

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
    (if ( not target-window )
        (message "not find target window")
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

(defun set-proxy()
  (interactive)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890")))
  )

(provide 'zo-tool)
