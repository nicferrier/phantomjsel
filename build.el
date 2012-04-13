;; Assists with testing packaged lisp

(let ((tar-package
       (concat
        (file-name-directory
         (or (buffer-file-name)
             load-file-name))
        (car (reverse command-line-args)))))
  (package-install-file tar-package))

;; End
