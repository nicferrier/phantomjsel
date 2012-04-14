;; Sets the package store to a dummy location

(setq package-user-dir
      (concat
       (file-name-directory
        (or (buffer-file-name)
            load-file-name
            default-directory))
       ".elpa"))
(load-file "build.el")
(ert-run-tests-batch-and-exit)

;; End
