;;; org-cite-overlay.el --- Overlays for org-cite citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/org-cite-overlay
;; Keywords: bib, tex
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (citeproc "0.9.4"))

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

;; This code is *heavily* inspired by org-fragtog (https://github.com/io12/org-fragtog).

;;; Code:

(require 'org)
(require 'org-element)
(require 'citeproc)
(require 'cl-lib)


;;; Customization

(defgroup org-cite-overlay nil
  "Display org-cite citations as formatted overlays."
  :group 'org-cite
  :prefix "org-cite-overlay-"
  :link '(url-link :tag "Sourcehut" "https://git.sr.ht/~swflint/org-cite-overlay")
  :link '(emacs-library-link :tag "Library Source" "org-cite-overlay.el"))

(defcustom org-cite-overlay-ignore-predicates (list 'org-at-table-p
                                                    'org-at-table.el-p
                                                    'org-at-block-p)
  "List of predicates to determine whether to ignore a citation.

These will be run until a non-nil value is returned, indicating
that the location should be ignored."
  :group 'org-cite-overlay
  :type 'hook)

(defcustom org-cite-overlay-delay 0.0
  "Seconds of delay before previewing citation."
  :group 'org-cite-overlay
  :type 'number)


;;; State Management

(defvar-local org-cite-overlay-processor nil
  "Citation processor for the current buffer.")

(defvar-local org-cite-overlay--timer nil
  "Current overlay timer.")


;;; Overlay Creation/Deletion

(defvar org-cite-overlay-proto nil
  "Prototype of org-cite-overlay.")
(put 'org-cite-overlay-proto 'face 'org-cite)

(defun org-cite-overlay--overlays-in (start end)
  "Get citation overlays in START to END."
  (when-let ((overlays (overlays-in start end)))
    (cl-remove-if (lambda (overlay)
                    (not (eq 'org-cite-overlay-proto (overlay-get overlay 'category))))
                  overlays)))

(defun org-cite-overlay--remove-overlay (start end)
  "Remove org-cite-overlays between START and END."
  (remove-overlays start end 'category 'org-cite-overlay-proto))

(defun org-cite-overlay--remove-all-overlays ()
  "Remove all org-cite-overlays in the buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'org-cite-overlay-proto))

(defun org-cite-overlay--remove-overlay-at-point ()
  "Delete the citation overlay at point."
  (mapcar #'delete-overlay (org-cite-overlay--overlays-in (1- (point)) (1+ (point)))))

(defun org-cite-overlay--create-overlay (start end overlay-content)
  "Insert OVERLAY-CONTENT as an overlay from START to END.

Note: OVERLAY-CONTENT can be fontified/have text properties/faces
attached; these will be shown as appropriate."
  (org-cite-overlay--remove-overlay start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'category 'org-cite-overlay-proto)
    (overlay-put overlay 'display overlay-content)
    overlay))




;; Bibliograpy Management

(defun org-cite-overlay--get-citations ()
  "Get citations for current buffer."
  (org-element-map (org-element-parse-buffer) 'citation #'identity))

(defun org-cite-overlay--citation-to-citeproc (citation)
  (citeproc-citation-create
   :cites (org-element-map citation 'citation-reference
            (lambda (cite)
              (cl-remove-if #'null (list (cons 'id (org-element-property :key cite))
                                         (cons 'prefix (car (org-element-property :prefix cite)))
                                         (cons 'suffix (car (org-element-property :suffix cite))))
                            :key #'cdr)))))

(defun org-cite-overlay--fill-processor-and-create-overlays ()
  (when-let* ((locale-getter (org-cite-csl--locale-getter))
              (item-getter (citeproc-hash-itemgetter-from-any
                            (mapcar #'expand-file-name (org-cite-list-bibliography-files))))
              (citations (org-cite-overlay--get-citations))
              (style (org-cite-csl--style-file nil))
              (processor (citeproc-create style item-getter locale-getter)))
    (org-cite-overlay--remove-all-overlays)
    (citeproc-append-citations (mapcar #'org-cite-overlay--citation-to-citeproc citations)
                               processor)
    (cl-mapcar (lambda (citation-object text)
                 (let ((start (org-element-property :begin citation-object))
                       (end (- (org-element-property :end citation-object)
                               (org-element-property :post-blank citation-object))))
                   (unless (<= start (point) end)
                     (org-cite-overlay--create-overlay start eend
                                                       (with-temp-buffer
                                                         (insert text)
                                                         (org-mode)
                                                         (font-lock-ensure)
                                                         (buffer-string))))))
               citations
               (citeproc-render-citations processor 'org 'no-links))
    (setq-local org-cite-overlay-processor
                processor)))


;;; Minor Mode

(define-minor-mode org-cite-overlay-mode
  "A minor mode to show formatted org-cite citations using overlays.

Previews are disabled when cursor is within them, and re-enabled
when the cursor leaves."
  :lighter " OCO"
  (if org-cite-overlay-mode
      (if (derived-mode-p 'org-mode)
          (message "Enabled Case")
        (display-warning 'org-cite-overlay
                         (substitute-quotes "`org-cite-overlay' may only be enabled in an `org-mode' buffer")
                         :error))
    (message "Disabled Case")))

(provide 'org-cite-overlay)

;;; org-cite-overlay.el ends here
