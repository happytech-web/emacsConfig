(defun file-name-only ()
  "Get the current buffer file name without directory."
  (file-name-nondirectory (buffer-name)))

(defun file-name-only-noext ()
  "Get the currennt buffer file name without directory and extension."
  (file-name-sans-extension (file-name-only)))

(defun compile-cpp-file ()
  "Simply compile a single cpp file."
  (interactive)
  (let* ((compile-command (format "g++ %s -g -o %s" (file-name-only) (file-name-only-noext))))

    (shell-command compile-command "*compilation*")))

(defun compile-and-run-cpp-file ()
  "Simply compile a single cpp file and then run it."
  (interactive)
  (let* ((compile-command (format "g++ %s -g -o %s" (file-name-only) (file-name-only-noext)))
         (run-command (format "./%s" (file-name-only-noext))))

    (shell-command compile-command "*compilation*")
    (async-shell-command run-command "*cpp-output*")))

(defun compile-c-file ()
  "Simply compile a single c file."
  (interactive)
  (let* ((compile-command (format "gcc %s -g -o %s" (file-name-only) (file-name-only-noext))))

    (shell-command compile-command)))

(defun compile-and-run-c-file ()
  "Simply compile a single c file and then run it."
  (interactive)
  (let* ((compile-command (format "gcc %s -g -o %s" (file-name-only) (file-name-only-noext)))
         (run-command (format "./%s" (file-name-only-noext))))

    (shell-command compile-command "*compilation*")
    (async-shell-command run-command "*c-output*")))

(provide 'init-cc)
;;; init-cc.el ends here
