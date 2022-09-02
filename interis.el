;;; interis.el --- Preview replace-regexp-in-string results -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://kisaragi-hiu.com/projects/interis
;; Keywords: convenience


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Preview a text replacement with `replace-regexp-in-string' similar
;; to https://gitlab.com/HiPhish/awk-ward.nvim. Enter your input text,
;; the regexp, the replacement, and the output text will be displayed
;; in real time.

;;; Code:

(require 'cl-lib)

(defun interis (&optional new)
  "Start an Interis buffer.

If NEW is non-nil (or interactively with a
\\[universal-argument]), always create a new buffer. Otherwise,
if a buffer already exists, just switch to it. This is the same
behavior as EWW or Eshell."
  (interactive "P")
  (let ((buffer (funcall (if new
                             #'generate-new-buffer
                           #'get-buffer-create)
                         "*Interis*")))
    (with-current-buffer buffer
      (interis--initialize))
    (pop-to-buffer buffer)))

(defvar interis-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "a" #'interis-test)
    map)
  "Keymap for Interis.")

(defvar-local interis--output-marker nil
  "Buffer local variable used to track the start of the output section.")
(defvar-local interis--last-output nil
  "Buffer local variable used to cache the previous output.")

(defun interis--propertize-read-only (string inhibit-front inhibit-rear &rest extra-properties)
  "Propertize STRING as read-only text.
If INHIBIT-FRONT is non-nil, disallow editing immediately before.
If INHIBIT-REAR is non-nil, disallow editing immediately after.

If EXTRA-PROPERTIES is non-nil, they are also passed to
`propertize' for convenience."
  (declare (indent 3))
  (apply #'propertize
         string
         'read-only t
         ;; See (info "(elisp)Sticky Properties").
         ;; Front-sticky prohibits editing here:
         ;;    |abc
         ;; while rear-sticky prohibits editing here:
         ;;    abc|
         ;; The default is rear-sticky & not front-sticky.
         'front-sticky (and inhibit-front '(read-only))
         'rear-nonsticky (and (not inhibit-rear)
                              '(read-only))
         extra-properties))

(cl-defun interis--initialize ()
  "Initialize the Interis buffer."
  (when (eq major-mode 'interis-mode)
    (cl-return-from interis--initialize))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq major-mode 'interis-mode
          mode-name "Interis")
    (setq-local revert-buffer-function (lambda (&rest _)
                                         (setq major-mode nil)
                                         (interis--initialize)))
    (use-local-map interis-map)
    (add-hook 'post-command-hook #'interis--update-output nil t)
    (save-excursion
      (insert
       (interis--propertize-read-only "Welcome to Interis, the interactive `replace-regexp-in-string' previewer.

" t t)
       (interis--propertize-read-only "Input text:" t t
         'face 'bold
         'interis-section 'input-text)
       (interis--propertize-read-only "\n" t nil)
       (interis--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interis--propertize-read-only "Regexp:" t t
         'face 'bold
         'interis-section 'regexp)
       (interis--propertize-read-only "\n" t nil)
       (interis--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interis--propertize-read-only "Replacement:" t t
         'face 'bold
         'interis-section 'replacement)
       (interis--propertize-read-only "\n" t nil)
       (interis--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interis--propertize-read-only "Output:\n" t t
         'face 'bold
         'interis-section 'output))
      (let ((m (point-marker)))
        (setq interis--output-marker m)))
    (run-mode-hooks 'interis-hook)))

(defun interis--update-output ()
  "Update the output section."
  (let ((output (replace-regexp-in-string
                 (interis--get-regexp)
                 (interis--get-replacement)
                 (interis--get-input-text)))
        (inhibit-read-only t)
        ;; Inhibit recording of undo information with a variable
        ;; called `buffer-undo-list' is just...
        (buffer-undo-list t))
    (unless (equal output interis--last-output)
      (save-excursion
        (goto-char interis--output-marker)
        (delete-region (point) (point-max))
        (insert output))
      (setq interis--last-output output))))

(defun interis--get-input-text ()
  "Get the current input text."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 2
     do (goto-char (next-single-property-change (point) 'interis-section)))
    (buffer-substring-no-properties
     ;; the 1+ and 1- cancel out the newlines
     (1+ (point))
     (1- (next-single-property-change (point) 'interis-section)))))

(defun interis--get-regexp ()
  "Get the current regexp."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 4
     do (goto-char (next-single-property-change (point) 'interis-section)))
    (buffer-substring-no-properties
     (1+ (point))
     (1- (next-single-property-change (point) 'interis-section)))))

(defun interis--get-replacement ()
  "Get the current replacement."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 6
     do (goto-char (next-single-property-change (point) 'interis-section)))
    (buffer-substring-no-properties
     (1+ (point))
     (1- (next-single-property-change (point) 'interis-section)))))

(provide 'interis)

;;; interis.el ends here
