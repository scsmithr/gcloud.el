;;; gcloud-process.el --- gcloud process -*- lexical-binding: t; -*-

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

(eval-when-compile
  (require 'subr-x))

(defvar gcloud-command "gcloud"
  "Path to gcloud binary.")

(defun gcloud-run (&rest args)
  "Execute a gcloud command with ARGS."
  (let ((command (concat gcloud-command " -q " (string-join args " "))))
    (message command)
    (shell-command-to-string command)))

(defun gcloud-current-project ()
  "Get the current gcloud project."
  (interactive)
  (string-trim (gcloud-run "config" "list" "--format='value(core.project)'")))

(defun gcloud-switch-project (project)
  "Switch to a different gcloud PROJECT."
  (interactive (list (let ((curr (gcloud-current-project)))
                       (read-string (format "Switch to project (default: %s): " curr) nil nil curr))))
  (gcloud-run "config" "set" "project" project))

(defun gcloud-current-account ()
  "Get the current gcloud account."
  (string-trim (gcloud-run "config" "list" "--format='value(core.account)'")))

(defun gcloud-switch-account (account)
  "Switch to a different gcloud ACCOUNT."
  (interactive (list (let ((curr (gcloud-current-account)))
                       (read-string (format "Switch account (default: %s): " curr) nil nil curr))))
  (gcloud-run "config" "set" "account" account))

(provide 'gcloud-process)
;;; gcloud-process.el ends here
