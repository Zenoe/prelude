(require 'zo-lib)

(define-advice command-line-1 (:after (&rest _) run-after-init-hook)
  (doom-run-hooks 'doom-after-init-hook))

(provide 'zo)
