(defun update-all-autoloads ()
  (interactive)
  (cd init-base-dir)
  (let ((generated-autoload-file
         (expand-file-name "autoload/autoload-gen.el")))
    (make-directory-autoloads '("lisp/lib" "search" "modules/workspaces/autoload") generated-autoload-file)
    ))


(defun set-proxy()
  (interactive)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890")))
  )

(provide 'zo-tool)
