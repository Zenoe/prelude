(defun update-all-autoloads ()
  (interactive)
  (cd user-emacs-directory )
  (let ((generated-autoload-file
         (expand-file-name "autoload/myloaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect
                            generated-autoload-file)
        (insert ";;")
        (save-buffer)))
    (mapcar #'update-directory-autoloads
            '("lisp" "search"))))
