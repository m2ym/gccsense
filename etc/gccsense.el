;;; gccsense.el --- GCCSense client for Emacs

;; Copyright (C) 2010  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: completion, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defgroup gccsense nil
  "GCCSense."
  :group 'completion
  :prefix "gccsense-")

(defcustom gccsense-gccrec-program "gccrec"
  "Path to gccrec program."
  :type 'string
  :group 'gccsense)

(defcustom gccsense-autopch-program "autopch"
  "Path to autopch program."
  :type 'string
  :group 'gccsense)

(defcustom gccsense-use-autopch t
  "Whether or not use autopch program. This may improve performance."
  :type 'boolean
  :group 'gccsense)

(defun gccsense-gccrec-command (filename tempfile &rest rest)
  (append `(,gccsense-gccrec-program
            "-r"
            ,@(if gccsense-use-autopch
                  (list "-p" gccsense-autopch-program))
            "-a"
            ,tempfile
            ,filename
            "-fsyntax-only")
          rest))

(defun gccsense-command-to-string (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process (car command) nil t nil (cdr command)))))

(defun gccsense-get-temp-name (filename)
  (concat (file-name-directory filename) ".gccsense." (file-name-nondirectory filename)))

(defun gccsense-parse-completion-string (string)
  (when (string-match "^completion: \\([^ ]*\\)\\(?: +\"\\([^\"]+\\)\"\\)?$" string)
    (let ((name (match-string 1 string))
          (decl (match-string 2 string)))
      (list name decl))))

(defun gccsense-get-completions (&optional buffer point)
  (or buffer (setq buffer (current-buffer)))
  (or point (setq point (point)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      (let* ((filename (buffer-file-name buffer))
             (tempfile (gccsense-get-temp-name filename))
             (line (line-number-at-pos))
             (column (1+ (current-column))))
        (write-region (point-min) (point-max) tempfile nil 0)
        (unwind-protect
            (delq nil
                  (mapcar 'gccsense-parse-completion-string
                          (split-string (gccsense-command-to-string
                                         (gccsense-gccrec-command
                                          filename
                                          tempfile
                                          (format "-code-completion-at=%s:%s:%s"
                                                  tempfile line column)))
                                        "\n")))
          (delete-file tempfile))))))

(defun gccsense-complete ()
  (interactive)
  (if (save-excursion (re-search-backward "\\(?:\\.\\|->\\|::\\)\\(.*\\)\\=" (line-beginning-position) t))
      (let* ((offset (match-beginning 1))
             (point (match-end 0))
             (prefix (match-string 1))
             (list (all-completions prefix
                                    (delete-dups (mapcar 'car
                                                         (gccsense-get-completions (current-buffer)
                                                                                   offset)))))
             (common (try-completion prefix list))
             (buffer "*Completions*"))
        (when (and (stringp common)
                   (not (equal prefix common)))
          (delete-region offset point)
          (insert common)
          (setq prefix common))
        (cond
         ((null list)
          (message "No completions"))
         ((eq (length list) 1)
          (let ((window (get-buffer-window buffer)))
            (if window
                (with-selected-window window
                  (or (window-dedicated-p window)
                      (bury-buffer))))))
         (t
          (with-output-to-temp-buffer buffer
            (display-completion-list list prefix))
          (display-buffer buffer))))))



;;;; Flymake

(defun gccsense-flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (command (gccsense-gccrec-command buffer-file-name temp-file)))
    (list (car command) (cdr command))))

(defun gccsense-flymake-setup ()
  (interactive)
  (require 'flymake)
  (push '("\\.\\(?:c\\|cc\\|cpp\\|cxx\\|C\\|CC\\)$" gccsense-flymake-init) flymake-allowed-file-name-masks))



;;;; Auto Complete Mode

(defvar ac-source-gccsense-member
  '((candidates . (gccsense-get-completions nil ac-point))
    (prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . (lambda (item) (car item)))
    (requires . 0)
    (symbol . "m")
    (cache)))

(defvar ac-source-gccsense-static-member
  '((candidates . (gccsense-get-completions nil ac-point))
    (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . (lambda (item) (car item)))
    (requires . 0)
    (symbol . "M")
    (cache)))

(provide 'gccsense)
;;; gccsense.el ends here
