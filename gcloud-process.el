;;; gcloud-process.el --- gcloud process -*- lexical-binding: t; -*-

;;; Code:

(defvar gcloud-command "gcloud"
  "Path to gcloud binary.")

(defun gcloud-run (&rest args)
  "Execute a gcloud command with ARGS."
  (let ((command (concat gcloud-command " " (strings-join args " "))))
    (message command)
    (shell-command-to-string command)))

(provide 'gcloud-process)
;;; gcloud-process.el ends here
