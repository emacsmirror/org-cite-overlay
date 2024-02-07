;;; org-cite-overlay.el --- Overlays for org-cite citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/org-cite-overlay
;; Keywords: bib, tex
;; Version: 0.5.0
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4"))

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
;; Automatically toggle showing org-cite citations as formatted
;; overlays upon cursor entry/exit.  To use, enable
;; `org-cite-overlay-mode' in org mode buffers (for example, in
;; `org-mode-hook').  This code is heavily inspired by org-fragtog
;; (https://github.com/io12/org-fragtog/).
;;
;; This package does have a few limitations:
;;  - Detection of exit or insertion of citations is somewhat
;;    inconsistent, so every 30 user commands in the buffer, all
;;    citations will be regenerated.
;;  - Only the default (chicago-author-year) style is used at the
;;    moment; this should eventually be fixed.

;;; Code:

(require 'org)
(require 'org-element)
(require 'oc-csl)
(require 'citeproc)
(require 'cl-lib)


;;; State Management

(defvar-local org-cite-overlay-processor nil
  "Citation processor for the current buffer.")

(defvar-local org-cite-overlay--timer nil
  "Current overlay timer.")


;;; Overlay Creation/Deletion

(defvar org-cite-overlay-proto nil
  "Prototype of org-cite-overlay.")
(put 'org-cite-overlay-proto 'face 'org-cite)
(put 'org-cite-overlay-proto 'evaporate t)
(put 'org-cite-overlay-proto 'modification-hooks (list (lambda (o &rest _ignore) (delete-overlay o))))

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
  "Generate a citeproc citation object from CITATION."
  (apply #'citeproc-citation-create
         (append (list :cites
                       (org-element-map citation 'citation-reference
                         (lambda (cite)
                           (cl-remove-if #'null
                                         (list (cons 'id (org-element-property :key cite))
                                               (cons 'prefix (and (org-element-property :prefix cite)
                                                                  (org-element-interpret-data (org-element-property :prefix cite))))
                                               (cons 'suffix (and (org-element-property :suffix cite)
                                                                  (org-element-interpret-data (org-element-property :suffix cite)))))
                                         :key #'cdr))))
                 (org-cite-csl--create-structure-params citation nil))))

(defun org-cite-overlay--fill-processor-and-create-overlays ()
  "Create a processor and overlays for the current buffer.

Note, the processor will be stored in
`org-cite-overlay-processor' which is buffer-local."
  (when-let* ((locale-getter (org-cite-csl--locale-getter))
              (item-getter (citeproc-hash-itemgetter-from-any
                            (mapcar #'expand-file-name (org-cite-list-bibliography-files))))
              (citations (org-cite-overlay--get-citations))
              (style (org-cite-csl--style-file nil)) ;TODO: get citation style from file.
              (processor (citeproc-create style item-getter locale-getter)))
    (org-cite-overlay--remove-all-overlays)
    (citeproc-append-citations (mapcar #'org-cite-overlay--citation-to-citeproc citations)
                               processor)
    (cl-mapcar (lambda (citation-object text)
                 (let ((start (org-element-property :begin citation-object))
                       (end (- (org-element-property :end citation-object)
                               (org-element-property :post-blank citation-object))))
                   (unless (<= start (point) end)
                     (org-cite-overlay--create-overlay start end
                                                       (with-temp-buffer
                                                         (insert text)
                                                         (org-mode)
                                                         (font-lock-ensure)
                                                         (buffer-string))))))
               citations
               (citeproc-render-citations processor 'org 'no-links))
    (setq-local org-cite-overlay-processor
                processor)))


;;; Cursor Motion Handling

(defvar-local org-cite-overlay--last-overlay-bounds nil
  "Bounds of the last overlay point was in.")

(defvar-local org-cite-overlay--command-counter 0
  "How many commands have been run before a forced overlay regen.")

(defun org-cite-overlay--post-command-function ()
  "This is executed by `post-command-hook', to remove/add overlays."
  (setq-local org-cite-overlay--command-counter (mod (1+ org-cite-overlay--command-counter) 30))
  (cond
   ((org-cite-overlay--overlays-in (1- (point)) (1+ (point)))
    (let ((overlay (car (org-cite-overlay--overlays-in (1- (point)) (1+ (point))))))
      (setq-local org-cite-overlay--last-overlay-bounds (cons (overlay-start overlay)
                                                              (overlay-end overlay)))
      (org-cite-overlay--remove-overlay-at-point)))
   ((and org-cite-overlay--last-overlay-bounds
         (not (<= (car org-cite-overlay--last-overlay-bounds) (point) (cdr org-cite-overlay--last-overlay-bounds))))
    (setq-local org-cite-overlay--last-overlay-bounds nil)
    (org-cite-overlay--fill-processor-and-create-overlays))
   ((= 0 org-cite-overlay--command-counter)
    (org-cite-overlay--fill-processor-and-create-overlays))))


;;; Minor Mode

(define-minor-mode org-cite-overlay-mode
  "A minor mode to show formatted org-cite citations using overlays.

Previews are disabled when cursor is within them, and re-enabled
when the cursor leaves."
  :lighter " OCO"
  (if org-cite-overlay-mode
      (if (derived-mode-p 'org-mode)
          (progn
            (org-cite-overlay--fill-processor-and-create-overlays)
            (add-hook 'post-command-hook #'org-cite-overlay--post-command-function nil t))
        (display-warning 'org-cite-overlay
                         "`org-cite-overlay' may only be enabled in an `org-mode' buffer"
                         :error)
        (setq-local org-cite-overlay-mode nil))
    (org-cite-overlay--remove-all-overlays)
    (remove-hook 'post-command-hook #'org-cite-overlay--post-command-function t)))

(provide 'org-cite-overlay)

;;; org-cite-overlay.el ends here
