;;; gcloud-instance.el --- gcloud instance -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Sean Smith <scsmithr@gmail.com>

;; This file is part of gcloud.el.

;; gcloud.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; gcloud.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with gcloud.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

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
    (message "buffer: %s" eshell-buffer-name)
    (eshell)))

(defun gcloud-instance-shell (ins)
  "Open shell in INS."
  (interactive (list (gcloud-read-instance)))
  (let* ((tramp-path (gcloud-format-tramp ins))
         (default-directory tramp-path))
    (shell (generate-new-buffer-name (format "*shell %s*" default-directory)))))

(provide 'gcloud-instance)
;;; gcloud-process.el ends here
