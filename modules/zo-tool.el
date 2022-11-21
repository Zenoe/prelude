(use-package pcre2el
  )

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

(provide 'zo-tool)
