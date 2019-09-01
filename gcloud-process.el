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

(defvar gcloud-command "gcloud"
  "Path to gcloud binary.")

(defun gcloud-run (&rest args)
  "Execute a gcloud command with ARGS."
  (let ((command (concat gcloud-command " " (strings-join args " "))))
    (message command)
    (shell-command-to-string command)))

(provide 'gcloud-process)
;;; gcloud-process.el ends here
