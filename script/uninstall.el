;;; emacs --batch --script $0 1.0.0

(package-initialize)
(let* ((package-table
               (mapcar
                (lambda (p) (cons (package-desc-full-name p) p))
                (delq nil
                      (mapcar (lambda (p) (unless (package-built-in-p p) p))
                              (apply #'append (mapcar #'cdr package-alist)))))))
  (package-delete (cdr (assoc (elt argv 0) package-table))))
