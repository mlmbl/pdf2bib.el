;;; pdf2bib.el --- An Emacs Lisp wrapper for the pdf2bib Python command-line tool -*- lexical-binding: t -*-

;; Copyright (C) 2025 mlmbl (Yuji TAKENOSHITA)

;; Author: Yuji TAKENOSHITA <yuji.takenoshita@gmail.com>
;; URL: https://github.com/mlmbl/pdf2bib.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools, bib, bibtex
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; This package provides an Emacs interface to the pdf2bib Python tool,
;; which extracts bibliographic information from PDF files and generates
;; BibTeX entries.
;;
;; Main features:
;; - Extract and insert BibTeX entries from PDF files
;; - Generate custom BibTeX keys (format: author+year+random)
;; - Add PDFs to .bib files with duplicate DOI checking
;; - Context-aware PDF selection (dired, pdf-tools integration)
;;
;; Requirements:
;; - Python 3.x with pdf2bib package installed
;; - pdf2bib command available in PATH
;;
;; Usage:
;;   M-x pdf2bib-insert-bibtex-from-pdf  - Insert entry at point
;;   M-x pdf2bib-regenerate-bibtex-key   - Regenerate key for current entry
;;   M-x pdf2bib-add-pdf-to-bib          - Add PDF to bibliography file
;;
;; Configuration:
;;   (setq pdf2bib-default-bib-file "~/bibliography.bib")
;;   (setq pdf2bib-python-command "pdf2bib")

;;; Code:

;;; Requirements

(require 'bibtex)

;; Silence byte-compiler warnings for optional dependencies
(declare-function dired-get-filename "dired")

;;; Customization

(defgroup pdf2bib nil
  "Interface to pdf2bib Python tool for BibTeX generation."
  :group 'convenience
  :prefix "pdf2bib-")

(defcustom pdf2bib-default-bib-file nil
  "Default BibTeX file for inserting entries.
If nil, prompts for a file each time."
  :type '(choice (const :tag "Always prompt" nil)
                 (file :tag "Default .bib file"))
  :group 'pdf2bib)

(defcustom pdf2bib-python-command "pdf2bib"
  "Command to run pdf2bib.
Can be a full path or just the command name if it's in PATH."
  :type 'string
  :group 'pdf2bib)

;;; Internal Utility Functions

(defun pdf2bib--random-letters (n)
  "Generate N random lowercase letters."
  (let ((letters "abcdefghijklmnopqrstuvwxyz")
        (result ""))
    (dotimes (_ n)
      (setq result (concat result
                           (string (aref letters (random (length letters)))))))
    result))

(defun pdf2bib--extract-first-author-lastname (author-string)
  "Extract the last name of the first author from AUTHOR-STRING.
Handles formats like:
  - \"Lennon, John and McCartney, Paul\"
  - \"John Lennon and Paul McCartney\"
  - \"Lennon, J. and McCartney, P.\""
  (when author-string
    (let* ((first-author (car (split-string author-string " and " t)))
           (first-author (string-trim first-author)))
      (cond
       ;; Format: "Lastname, Firstname"
       ((string-match "\\([^,]+\\)," first-author)
        (match-string 1 first-author))
       ;; Format: "Firstname Lastname"
       ((string-match "\\s-+\\([^ ]+\\)\\s-*$" first-author)
        (match-string 1 first-author))
       ;; Fallback: use the whole string
       (t first-author)))))

(defun pdf2bib--get-pdf-file ()
  "Get PDF file from context (dired or pdf-tools) or prompt user.
Returns the PDF file path."
  (cond
   ;; Currently viewing in pdf-tools
   ((and (eq major-mode 'pdf-view-mode)
         (boundp 'buffer-file-name)
         buffer-file-name)
    buffer-file-name)
   
   ;; Cursor is on a PDF file in dired buffer
   ((and (eq major-mode 'dired-mode)
         (dired-get-filename nil t)
         (string-match-p "\\.pdf\\'" (dired-get-filename nil t)))
    (dired-get-filename nil t))
   
   ;; Otherwise: prompt user for input
   (t
    (read-file-name "PDF file: " nil nil t nil
                    (lambda (name) (string-match-p "\\.pdf\\'" name))))))

(defun pdf2bib--find-entry-by-doi (doi bib-file)
  "Search for BibTeX entry with DOI in BIB-FILE.
Returns the position of the entry if found, nil otherwise."
  (when (and doi (not (string-empty-p doi)))
    (with-current-buffer (find-file-noselect bib-file)
      (save-excursion
        (goto-char (point-min))
        (let ((search-doi (replace-regexp-in-string "[{}]" "" doi))
              (found-pos nil))
          (while (and (not found-pos)
                      (re-search-forward "^[ \t]*doi[ \t]*=[ \t]*[{\"]*\\([^,}\"]+\\)" nil t))
            (let ((current-doi (replace-regexp-in-string "[{}\" ]" "" (match-string 1))))
              (when (string= (downcase search-doi) (downcase current-doi))
                (setq found-pos (save-excursion
                                  (bibtex-beginning-of-entry)
                                  (point))))))
          found-pos)))))

;;; Public API - BibTeX Key Generation

(defun pdf2bib-generate-bibtex-key ()
  "Generate a new BibTeX key based on author and year.
Format: lastname + year + \"-\" + two random lowercase letters.
Example: lennon1967-gb"
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((entry (bibtex-parse-entry t))
           (author (bibtex-text-in-field "author"))
           (year (bibtex-text-in-field "year"))
           (lastname (pdf2bib--extract-first-author-lastname author))
           (random-suffix (pdf2bib--random-letters 2)))
      (if (and lastname year)
          (format "%s%s-%s"
                  (downcase lastname)
                  year
                  random-suffix)
        (error "Could not extract author or year from entry")))))

;;;###autoload
(defun pdf2bib-regenerate-bibtex-key ()
  "Regenerate BibTeX key for the entry at point."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((old-key (bibtex-key-in-head))
           (new-key (pdf2bib-generate-bibtex-key)))
      (if old-key
          (progn
            ;; Get entry type (@article, @book, etc.)
            (bibtex-beginning-of-entry)
            (looking-at "@\\([a-zA-Z]+\\){")
            (let ((entry-type (match-string 1)))
              ;; Replace old key with new key
              (search-forward (concat "{" old-key))
              (replace-match (concat "{" new-key))
              (message "BibTeX key updated: %s → %s" old-key new-key)))
        (error "No BibTeX entry found at point")))))

;;; Public API - PDF to BibTeX Conversion

;;;###autoload
(defun pdf2bib-insert-bibtex-from-pdf (pdf-file)
  "Insert BibTeX entry at point from PDF using pdf2bib.
Prompts for PDF-FILE if not provided."
  (interactive "fPDF file: ")
  (let* ((pdf-path (expand-file-name pdf-file))
         (output (string-trim
                  (shell-command-to-string
                   (format "%s %s | tail -n +2" 
                           pdf2bib-python-command
                           (shell-quote-argument pdf-path))))))
    (if (or (string-empty-p output)
            (string-match-p "error\\|warning\\|failed" (downcase output)))
        (message "No bib-info found for: %s" (file-name-nondirectory pdf-path))
      (insert output)
      (bibtex-mode)
      (bibtex-clean-entry)
      (message "BibTeX entry inserted from: %s" (file-name-nondirectory pdf-path)))))

;;;###autoload
(defun pdf2bib-add-pdf-to-bib (pdf-file bib-file)
  "Add BibTeX entry from PDF-FILE to BIB-FILE with custom key.
PDF-FILE is determined by context (dired, pdf-tools, or prompt).
BIB-FILE defaults to `pdf2bib-default-bib-file' or prompts if nil.
Checks for duplicate DOI before adding."
  (interactive
   (list (pdf2bib--get-pdf-file)
         (or pdf2bib-default-bib-file
             (and (boundp 'org-cite-global-bibliography)
                  (car org-cite-global-bibliography))
             (read-file-name "BibTeX file: " nil nil t nil
                             (lambda (name) (string-match-p "\\.bib\\'" name))))))
  (let ((pdf-path (expand-file-name pdf-file))
        (bib-path (expand-file-name bib-file)))
    ;; Try to generate BibTeX in a temporary buffer
    (with-temp-buffer
      (let ((output (string-trim
                     (shell-command-to-string
                      (format "%s %s | tail -n +2" 
                              pdf2bib-python-command
                              (shell-quote-argument pdf-path))))))
        (if (or (string-empty-p output)
                (string-match-p "error\\|warning\\|failed" (downcase output)))
            (progn
              (message "No bib-info found for: %s" (file-name-nondirectory pdf-path))
              nil) ; Exit on parse failure
          ;; Parse successful: Insert BibTeX entry
          (insert output)
          (bibtex-mode)
          (bibtex-clean-entry)
          
          ;; Extract DOI
          (goto-char (point-min))
          (bibtex-beginning-of-entry)
          (let* ((entry (bibtex-parse-entry t))
                 (doi (bibtex-text-in-field "doi" entry))
                 (existing-entry (when doi (pdf2bib--find-entry-by-doi doi bib-path))))
            
            ;; Check for duplicates
            (if existing-entry
                (progn
                  (find-file bib-path)
                  (goto-char existing-entry)
                  (bibtex-beginning-of-entry)
                  (message "Entry with DOI '%s' already exists in %s" 
                           doi 
                           (file-name-nondirectory bib-path)))
              
              ;; No duplicate: Regenerate key and add
              (goto-char (point-min))
              (bibtex-beginning-of-entry)
              (let* ((old-key (bibtex-key-in-head))
                     (new-key (pdf2bib-generate-bibtex-key)))
                (when old-key
                  (search-forward (concat "{" old-key))
                  (replace-match (concat "{" new-key)))
                
                ;; Add to end of bib file
                (let ((entry-text (buffer-string)))
                  (find-file bib-path)
                  (goto-char (point-max))
                  ;; Add newline if not at beginning of line
                  (unless (bolp) (insert "\n"))
                  (insert "\n" entry-text "\n")
                  (bibtex-mode)
                  (save-buffer)
                  (message "Added entry with key '%s' to %s" 
                           new-key 
                           (file-name-nondirectory bib-path))
                  ;; Display bib file
                  (switch-to-buffer (current-buffer))
                  (goto-char (point-max))
                  (bibtex-beginning-of-entry))))))))))

;;; Provide

(provide 'pdf2bib)
;;; pdf2bib.el ends here
