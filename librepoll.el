;;; librepoll.el --- Librepoll Emacs interface.      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Pierre-Antoine Rouby

;; Author: Pierre-Antoine Rouby <contact@parouby.fr>
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
(require 'cl)

(defvar librepoll-option-format)
(setq librepoll-option-format
      " - **%s %d votes**; [%s] %d%% *C-c %s*\n")

;;; Progress bar
(defconst MAX-BAR-SIZE 10)

(defun lp-percentage (for total)
  "Returns percentage in float."
  (* (/ (float for) (float total)) 100))

(defun lp-progress-bar-aux (rest char)
  (if (eq rest 0)
      ""
    (concat char (lp-progress-bar-aux (- rest 1) char))))

(defun lp-progress-bar (percentage)
  "Returns progress bar text for PERCENTAGE."
  (let* ((num (/ (round percentage) 10))
         (rnum (* num (/ MAX-BAR-SIZE 10))))
    (concat (lp-progress-bar-aux rnum "-")
            (lp-progress-bar-aux (- MAX-BAR-SIZE rnum) " "))))


;;;###autoload
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

;;;###autoload
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
                       (lambda (&key &allow-other-keys)
                         (message "Vote: Ok"))))
    (librepoll-poll instance poll)))

(defun lp-compute-total (opts)
  "Return total of vote for OPTS."
  (cond
   ((null opts) 0)
   (t (let* ((h    (car opts))
             (vote (aref (cdr h) 1))) ;Get vote '(id . ["txt" vote])
        (+ vote (lp-compute-total (cdr opts)))))))

(defun lp-map-options (opts instance poll)
  "Map options, set local key and return list of option line to
display."
  (let ((total (lp-compute-total opts)))
    (mapcar* (lambda (l c)
               (let* ((id   (car l))
                      (tab  (cdr l))
                      (txt  (aref tab 0))
                      (vote (aref tab 1)))
                 ;; Bind key
                 (local-set-key (kbd (format "C-c %s" c))
                                (lambda (yesno)
                                  (interactive "sVote (yes/no): ")
                                  (when (string= yesno "yes")
                                    (librepoll-vote instance poll
                                                    (string-to-number
                                                     (format "%s" id))))))
                 ;; Format
                 (format librepoll-option-format txt vote
                         (lp-progress-bar (lp-percentage vote total))
                         (lp-percentage vote total)
                         c)))
             opts
             (list "a" "b" "c" "d" "e" "f" "g" "h"
                   "i" "j" "k" "l" "m" "n" "o" "p"
                   "q" "r" "s" "t" "u" "v" "w" "x"
                   "y" "z" "1" "2" "3" "4" "5" "6"
                   "7" "8" "9" "0"))))

;;;###autoload
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
                           (insert description "\n\n")
                           ;; Options
                           (apply 'insert
                                  (lp-map-options options instance poll))
                           ;; Refresh buffer
                           (local-set-key (kbd "C-c C-r")
                                          (lambda ()
                                            (interactive)
                                            (librepoll-poll instance poll)))
                           (insert "\n\n*Update buffer: C-c C-r*")
                           ;; Read only
                           (read-only-mode t)))))
    t))

(provide 'librepoll)
;;; librepoll.el ends here
