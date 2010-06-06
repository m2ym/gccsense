;;; gccsense.el --- GCCSense client for GNU Emacs

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

(defcustom gccsense-ctagsfind-program "ctagsfind"
  "Path to ctagsfind program."
  :type 'string
  :group 'gccsense)

(defcustom gccsense-c-driver "gcc-gdt"
  "Path to C compiler driver."
  :type 'string
  :group 'gccsense)

(defcustom gccsense-c++-driver "g++-gdt"
  "Path to C++ compiler driver."
  :type 'string
  :group 'gccsense)

(defcustom gccsense-use-autopch t
  "Whether or not use autopch program. This may improve performance."
  :type 'boolean
  :group 'gccsense)

(defvar gccsense-last-command-temp-name nil
  "Temporary file name for the last command to execute.")

(defvar gccsense-last-command-real-name nil
  "Real file name for the last command to execute.")

(defvar gccsense-identifier-syntax-table
  (let ((table (copy-syntax-table c-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" table)
    (modify-syntax-entry ?> ")<" table)
    table))

(defun gccsense-get-temp-name (filename)
  (concat (file-name-directory filename) ".gccsense." (file-name-nondirectory filename)))

(defun gccsense-current-column ()
  "Return the current column which can be read by GCC."
  (1+ (- (point) (line-beginning-position))))

(defun gccsense-message (format &rest args)
  (if args
      (setq format (format format args)))
  (if (fboundp 'popup-tip)
      (popup-tip format :margin t)
    (message "%s" format)))

(defun gccsense-strip-template-parameters (name)
  (with-temp-buffer
    (insert name)
    (goto-char (point-min))
    (with-syntax-table gccsense-identifier-syntax-table
      (while (not (eolp))
        (forward-word)
        (if (eq (char-after) ?<)
            (kill-sexp))))
    (buffer-string)))

(defun gccsense-device-string (string separators)
  (if (string-match separators string)
      (list (substring string 0 (match-beginning 0))
            (substring string (match-end 0)))
    (list string)))

(defun gccsense-vim-regexp-to-emacs-regexp (regexp)
  (replace-regexp-in-string "[*+]" "\\\\\\&" regexp))

(defun gccsense-gccrec-command (filename tempfile &rest rest)
  (append `(,gccsense-gccrec-program
            "-r"
            ,@(if gccsense-use-autopch
                  (list "-p" gccsense-autopch-program))
            "-d"
            ,(if (string-match "\(?:cpp\|cc\|cxx\|CPP\|CC\|CXX\)" filename)
                 gccsense-c++-driver
               gccsense-c-driver)
            "-a"
            ,tempfile
            ,filename
            "-fsyntax-only")
          rest))

(defun gccsense-command-to-string (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process (car command) nil t nil (cdr command)))))

(defun gccsense-buffer-command (buffer point command)
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      (let* ((filename (buffer-file-name buffer))
             (tempfile (gccsense-get-temp-name filename))
             (line (line-number-at-pos))
             (column (gccsense-current-column)))
        (setq gccsense-last-command-temp-name tempfile
              gccsense-last-command-real-name filename)
        (write-region (point-min) (point-max) tempfile nil 0)
        (unwind-protect
            (gccsense-command-to-string
             (gccsense-gccrec-command
              filename
              tempfile
              (format "-%s=%s:%s:%s" command tempfile line column)))
          (delete-file tempfile))))))

(defun gccsense-parse-completion-string (string)
  (if (string-match "^completion:" string)
      (cdr (split-string string "\t"))))

(defun gccsense-get-completions (&optional buffer point)
  (delq nil
        (mapcar 'gccsense-parse-completion-string
                (split-string (gccsense-buffer-command (or buffer (current-buffer))
                                                       (or point (point))
                                                       "-code-completion-at")
                              "\n"))))

(defun gccsense-parse-declaration-string (string)
  (if (string-match "^declaration:" string)
      (cdr (split-string string "\t"))))

(defun gccsense-get-declarations (&optional buffer point)
  (delq nil
        (mapcar 'gccsense-parse-declaration-string
                (split-string (gccsense-buffer-command (or buffer (current-buffer))
                                                       (or point (point))
                                                       "-declaration-at")
                              "\n"))))

(defun gccsense-open-declaration (decl)
  (when decl
    (let ((file (nth 2 decl))
          (dir (nth 3 decl))
          (line (string-to-number (nth 4 decl))))
      (unless (file-name-absolute-p file)
        (setq file (file-truename (concat dir "/" file))))
      (if (equal file gccsense-last-command-temp-name)
          (setq file gccsense-last-command-real-name))
      (if (file-exists-p file)
          (progn
            (find-file file)
            (goto-line line)
            (back-to-indentation))
        (gccsense-message "No such file: %s" file)))))

(defun gccsense-open-definition (def)
  (when def
    (let ((path (cadr def))
          (address (nth 2 def)))
      (find-file path)
      (cond
       ((string-match "^[0-9]+$" address)
        (goto-line (string-to-number address)))
       ((string-match "^/\\(.+\\)/$" address)
        (goto-char (point-min))
        (re-search-forward (gccsense-vim-regexp-to-emacs-regexp (match-string 1 address)) nil t)
        (back-to-indentation))
       ((string-match "^\\?\\(.+\\)\\?$" address)
        (goto-char (point-max))
        (re-search-backward (gccsense-vim-regexp-to-emacs-regexp (match-string 1 address)) nil t)
        (back-to-indentation))))))

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
          (gccsense-message "No completions"))
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

(defun gccsense-declaration-at-point ()
  (interactive)
  (let* ((decls (gccsense-get-declarations))
         (decl (mapconcat 'cadr decls "\n\n")))
    (gccsense-message (if decls decl "No declaration found"))))

(defun gccsense-open-declaration-at-point ()
  (interactive)
  (let ((decls (gccsense-get-declarations)))
    (if decls
        (gccsense-open-declaration
         (if (and (fboundp 'popup-menu*)
                  (> (length decls) 1))
             (popup-menu* (mapcar (lambda (decl)
                                    (let ((name (car decl))
                                          (file (nth 2 decl))
                                          (dir (nth 3 decl))
                                          (line (nth 4 decl)))
                                      (popup-make-item (format "%s (%s:%s)"
                                                               name
                                                               (file-truename (concat dir "/" file))
                                                               line)
                                                       :value decl)))
                                  decls)
                          :margin t
                          :scroll-bar t)
           (gccsense-open-declaration (car decls))))
      (gccsense-message "No declaration found"))))

(defun gccsense-open-definition-at-point ()
  (interactive)
  (let ((defs (apply 'append
                     (mapcar (lambda (decl)
                               (let ((qname (gccsense-strip-template-parameters (car decl))))
                                 (when (string-match "^\\(?:\\(.+\\)\\(?:::\\|\\.\\)\\)?\\(.+\\)$" qname)
                                   (setq name (match-string 2 qname))
                                   (if name
                                       (setq base (match-string 1 qname))
                                     (setq name (match-string 1 qname)))
                                   (delq nil
                                         (mapcar (lambda (def)
                                                   (let* ((fields (gccsense-device-string def "\t"))
                                                          (path (car fields))
                                                          (address (cadr fields)))
                                                     (if (and path address)
                                                         (list qname path address))))
                                                 (split-string (gccsense-command-to-string
                                                                (list gccsense-ctagsfind-program
                                                                      "-F" "kind=f"
                                                                      "-F" (concat "class=" (match-string 1 qname))
                                                                      (match-string 2 qname)))
                                                               "\n"))))))
                             (gccsense-get-declarations)))))
    (if defs
        (gccsense-open-definition
         (if (and (fboundp 'popup-menu*)
                  (> (length defs) 1))
             (popup-menu* (mapcar (lambda (def)
                                    (popup-make-item (format "%s (%s)" (car def) (cadr def))
                                                     :value def))
                                  defs)
                          :margin t
                          :scroll-bar t)
           (car defs)))
      (gccsense-message "No definition found"))))



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

(defun ac-complete-gccsense ()
  (interactive)
  (auto-complete '(ac-source-gccsense-member ac-source-gccsense-static-member)))



;;;; Diagnose

(defun gccsense-diagnose-error (msg)
  (switch-to-buffer "*GCCSense-Diagnose*")
  (erase-buffer)
  (save-excursion
    (insert msg))
  (error "Failed"))

(defmacro gccsense-diagnose-checklist (&rest form)
  `(condition-case nil
       (progn
         ,@(mapcar (lambda (pair)
                     (setq pair (macroexpand pair))
                     `(unless
                          (condition-case nil
                              (progn
                                ,(car pair))
                            (error))
                       (gccsense-diagnose-error ,(cadr pair))))
                   form)
         (message "Everything OK!"))
     (error)))

(defmacro gccsense-diagnose-check-program (path)
  `((eq (call-process ,path nil nil nil "--version") 0)
    ,(format "`%s' is not executable from Emacs or returned error.
Make sure that the program was correctly installed and can be run from terminal.
You may add a directory where the program was installed into `exec-path' variable."
             (symbol-value path))))

(defun gccsense-diagnose ()
  (interactive)
  (gccsense-diagnose-checklist
   (gccsense-diagnose-check-program gccsense-gccrec-program)
   (gccsense-diagnose-check-program gccsense-autopch-program)
   (gccsense-diagnose-check-program gccsense-c-driver)
   (gccsense-diagnose-check-program gccsense-c++-driver)

   ((and (not (string-match "unrecognized option" (gccsense-command-to-string (list gccsense-c-driver "-code-completion-at=x"))))
         (not (string-match "unrecognized option" (gccsense-command-to-string (list gccsense-c++-driver "-code-completion-at=x")))))
    (format "GCC driver can not take `-code-completion-at' option. Make sure that %s and %s
was installed correctly and `gccsense-c-driver' and `gccsense-c++-driver' points to that programs."
            gccsense-c-driver gccsense-c++-driver))

   ((progn
      (save-window-excursion
        (save-excursion
          (find-file-literally "/tmp/test-gccsense-diagnose.cpp")
          (erase-buffer)
          (insert "#include <string>
int main() {
std::string s;
s.
}")
          (save-buffer)
          (goto-line 4)
          (move-to-column 2)
          (assoc "c_str" (gccsense-get-completions)))))
    "Can not obtain completions for std::string.
You may not use code-completion.")))

(provide 'gccsense)
;;; gccsense.el ends here
