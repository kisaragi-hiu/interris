;;; interris.el --- Preview replace-regexp-in-string results -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://kisaragi-hiu.com/projects/interris
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

(defun interris (&optional new)
  "Start an Interris buffer.

If NEW is non-nil (or interactively with a
\\[universal-argument]), always create a new buffer. Otherwise,
if a buffer already exists, just switch to it. This is the same
behavior as EWW or Eshell."
  (interactive "P")
  (let ((buffer (funcall (if new
                             #'generate-new-buffer
                           #'get-buffer-create)
                         "*Interris*")))
    (with-current-buffer buffer
      (interris--initialize))
    (pop-to-buffer buffer)))

(defvar interris-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "a" #'interris-test)
    map)
  "Keymap for Interris.")

(defvar-local interris--output-marker nil
  "Buffer local variable used to track the start of the output section.")
(defvar-local interris--last-output nil
  "Buffer local variable used to cache the previous output.")

(defun interris--propertize-read-only (string inhibit-front inhibit-rear &rest extra-properties)
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

(cl-defun interris--initialize ()
  "Initialize the Interris buffer."
  (when (eq major-mode 'interris-mode)
    (cl-return-from interris--initialize))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq major-mode 'interris-mode
          mode-name "Interris")
    (setq-local revert-buffer-function (lambda (&rest _)
                                         (setq major-mode nil)
                                         (interris--initialize)))
    (use-local-map interris-map)
    (add-hook 'post-command-hook #'interris--update-output nil t)
    (save-excursion
      (insert
       (interris--propertize-read-only "Welcome to Interris, the interactive `replace-regexp-in-string' previewer.

" t t)
       (interris--propertize-read-only "Input text:" t t
         'face 'bold
         'interris-section 'input-text)
       (interris--propertize-read-only "\n" t nil)
       (interris--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interris--propertize-read-only "Regexp:" t t
         'face 'bold
         'interris-section 'regexp)
       (interris--propertize-read-only "\n" t nil)
       (interris--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interris--propertize-read-only "Replacement:" t t
         'face 'bold
         'interris-section 'replacement)
       (interris--propertize-read-only "\n" t nil)
       (interris--propertize-read-only "\n" nil t
         'face 'hl-line)
       (interris--propertize-read-only "Output:\n" t t
         'face 'bold
         'interris-section 'output))
      (let ((m (point-marker)))
        (setq interris--output-marker m)))
    (run-mode-hooks 'interris-hook)))

(defun interris--update-output ()
  "Update the output section."
  (let ((output (replace-regexp-in-string
                 (interris--get-regexp)
                 (interris--get-replacement)
                 (interris--get-input-text)))
        (inhibit-read-only t)
        ;; Inhibit recording of undo information with a variable
        ;; called `buffer-undo-list' is just...
        (buffer-undo-list t))
    (unless (equal output interris--last-output)
      (save-excursion
        (goto-char interris--output-marker)
        (delete-region (point) (point-max))
        (insert output))
      (setq interris--last-output output))))

(defun interris--get-input-text ()
  "Get the current input text."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 2
     do (goto-char (next-single-property-change (point) 'interris-section)))
    (buffer-substring-no-properties
     ;; the 1+ and 1- cancel out the newlines
     (1+ (point))
     (1- (next-single-property-change (point) 'interris-section)))))

(defun interris--get-regexp ()
  "Get the current regexp."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 4
     do (goto-char (next-single-property-change (point) 'interris-section)))
    (buffer-substring-no-properties
     (1+ (point))
     (1- (next-single-property-change (point) 'interris-section)))))

(defun interris--get-replacement ()
  "Get the current replacement."
  (save-excursion
    (goto-char (point-min))
    (cl-loop
     repeat 6
     do (goto-char (next-single-property-change (point) 'interris-section)))
    (buffer-substring-no-properties
     (1+ (point))
     (1- (next-single-property-change (point) 'interris-section)))))

(provide 'interris)

;;; interris.el ends here
