;;; librepoll.el --- Librepoll Emacs interface.      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Pierre-Antoine Rouby

;; Author: Pierre-Antoine Rouby <prouby@fry>
;; Keywords: extensions, lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'request)

(defvar librepoll-option-format)
(setq librepoll-option-format
      " - %s; %s; **%d votes**; *M-x librepoll-vote RET %s RET %d RET %s*\n")

(defun librepoll-instance-status (instance)
  "Display INSTANCE status in message buffer."
  (interactive "sInstance url: ")
  (let ((url (concat instance "/api/v1/status")))
    (request url
             :type "GET"
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (let ((status (assoc-default 'status data))
                               (license (assoc-default 'license data)))
                           (message "%s status: %s (%s)"
                                    instance status license)))))))

(defun librepoll-poll (instance poll)
  "Display librepoll poll."
  (interactive "sInstance url:
nPoll id: ")
  (let ((buffer (get-buffer-create (format "* %s-%d *" instance poll)))
        (url (format "%s/api/v1/poll/%d" instance poll)))
    (request url
             :type "GET"
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (let ((name        (assoc-default 'name        data))
                               (description (assoc-default 'description data))
                               (options     (assoc-default 'options     data)))
                           (set-buffer buffer)
                           (switch-to-buffer buffer)
                           (read-only-mode 0)
                           (markdown-mode)
                           (erase-buffer)
                           ;; Name & desc
                           (insert "# " name "\n\n")
                           (insert description "\n")
                           ;; Options
                           (apply 'insert
                                  (mapcar
                                   (lambda (l)
                                     (let* ((id   (car l))
                                            (tab  (cdr l))
                                            (txt  (aref tab 0))
                                            (vote (aref tab 1)))
                                       (format librepoll-option-format
                                               id txt vote instance poll id)))
                                   options))
                           ;; Read only
                           (read-only-mode t)))))
    t))

(defun librepoll-vote (instance poll opt)
  "Vote."
  (interactive "sInstance url:
nPoll:
nOption: ")
  (let ((url (format "%s/api/v1/vote/%d/%d" instance poll opt)))
    (request url
             :type "GET"
             :parser 'json-read
             :success (cl-function
                       (lambda (&key data &allow-other-keys)
                         (message "Vote: Ok"))))
    (librepoll-poll instance poll)))

(provide 'librepoll)
;;; librepoll.el ends here
