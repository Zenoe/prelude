(prelude-require-packages '(xclip general ripgrep pcre2el))

(defun update-all-autoloads ()
  (interactive)
  (cd "~/emacs/myemacs")
  (let ((generated-autoload-file
         (expand-file-name "autoload/myloaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect
                            generated-autoload-file)
        (insert ";;")
        (save-buffer)))
    (mapcar #'update-directory-autoloads
            '("lisp" "search"))))

(defun set-proxy()
  (interactive)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10\\..*\\|192\\.168\\..*\\)")
          ("http" . "127.0.0.1:7890")
          ("https" . "127.0.0.1:7890")))
  )

(provide 'zo-tool)
