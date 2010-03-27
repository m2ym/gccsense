(defvar gccsense-use-autopch t)

(defun gccsense-get-temp-name (filename)
  (concat (file-name-directory filename) ".gccsense." (file-name-nondirectory filename)))

(defun gccsense-parse-completion-string (string)
  (when (string-match "^completion: \\([^ ]*\\)\\(?: +\"\\([^\"]+\\)\"\\)?$" string)
    (let ((name (match-string 1 string))
          (decl (match-string 2 string)))
      (list name name decl))))

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
            (let ((command (format "gccrec -r %s -f %s %s -fsyntax-only -code-completion-at=%s:%s:%s"
                                   (if gccsense-use-autopch
                                       "-p autopch"
                                     "")
                                   tempfile
                                   filename
                                   tempfile line column)))
              (delq nil (mapcar 'gccsense-parse-completion-string
                                (split-string (shell-command-to-string command)
                                              "\n"))))
          (delete-file tempfile))))))

(ac-define-source gccsense-member
  '((candidates . (gccsense-get-completions nil ac-point))
    (prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . (lambda (item) (nth 1 item)))
    (requires . 0)
    (symbol . "m")
    (cache)))

(ac-define-source gccsense-static-member
  '((candidates . (gccsense-get-completions nil ac-point))
    (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (document . (lambda (item) (nth 1 item)))
    (requires . 0)
    (symbol . "M")
    (cache)))
