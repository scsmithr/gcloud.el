;;; gcloud-instance.el --- gcloud process -*- lexical-binding: t; -*-

;;; Code:

(require 'gcloud-process)

(defconst gcloud-tramp-method "gcloud")

;;;###autoload
(defun gcloud-tramp-add-method ()
  "Add gcloud to list of tramp methods."
  ;; TODO: Handle projects/zones.
  (add-to-list 'tramp-methods
               `(,gcloud-tramp-method
                 (tramp-login-program ,gcloud-command)
                 (tramp-login-args (("compute" "ssh") ("%h") ("--") ("/bin/bash")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-args ("-i" "-c")))))

;;;###autoload
(eval-after-load 'tramp
  '(progn (gcloud-tramp-add-method)))

(defun gcloud-read-instance ()
  "Prompt for an instance name."
  ;; TODO: Use completing-read with an existing list of instances.
  (read-string "Instance: "))

(defun gcloud-format-tramp (ins &optional path)
  (format "/%s:%s:%s" gcloud-tramp-method ins (or path "/")))

(defun gcloud-instance-find-directory (ins dir)
  "Open DIR inside of INS."
  (interactive (list (gcloud-read-instance)
                     (read-string "Directory: ")))
  (let ((tramp-path (gcloud-format-tramp ins dir)))
    (dired tramp-path)))

(defalias 'gcloud-instance-dired 'gcloud-instance-find-directory)

(defun gcloud-instance-find-file (ins file)
  "Open FILE inside of INS."
  (interactive (list (gcloud-read-instance)
                     (read-string "Path: ")))
  (find-file (gcloud-format-tramp ins file)))

(defun gcloud-instance-eshell (ins)
  "Open eshell in INS."
  (interactive (list (gcloud-read-instance)))
  ;; TODO: Allow specifying default dir.
  (let* ((tramp-path (gcloud-format-tramp ins))
         (default-directory tramp-path)
         (eshell-buffer-name (generate-new-buffer-name (format "*eshell %s*" default-directory))))
    (eshell)))

(defun gcloud-instance-shell (ins)
  "Open shell in INS."
  (interactive (list (gcloud-read-instance)))
  (let* ((tramp-path (gcloud-format-tramp ins))
         (default-directory tramp-path))
    (shell (generate-new-buffer-name (format "*shell %s*" default-directory)))))

(provide 'gcloud-instance)
;;; gcloud-process.el ends here
