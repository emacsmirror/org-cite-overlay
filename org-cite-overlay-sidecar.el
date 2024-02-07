;;; org-cite-overlay-sidecar.el --- Show Sidecar for overlaid org-cite citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/org-cite-overlay
;; Keywords: bib
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (citeproc "0.9.4") (org-cite-overlay "0.0.1") (universal-sidecar "1.5.0") (universal-sidecar-citeproc "1.0.0"))

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
;; This is meant to replace `org-cite-sidecar'
;; (https://melpa.org/#/org-cite-sidecar), and will integrate with
;; `org-cite-overlay'.  Similar to the former, it will show formatted
;; org-cite citations in the sidecar, however, if `org-cite-overlay'
;; is in use, the processor it fills will be used (reducing the amount
;; of computation performed).  Because it is compatible with buffers
;; not using `org-cite-overlay-mode', it is also necessary to
;; configure universal-sidecar-citeproc, as shown:
;;
;; (setq universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/"
;;       ;; set to your directories for locale and style data
;;       universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
;; (add-to-list 'universal-sidecar-sections 'org-cite-overlay-sidecar)
;;
;; Additionally, there are two arguments to the section which are not
;; exposed as customization variables:
;;
;; - `:style' allows you to select a prefered CSL style within
;;   `universal-sidecar-citeproc-styles'.  Default is
;;   `universal-sidecar-citeproc-default-style'.
;; - `:header' allows you to change the header of the section from the
;;   default "References".

;;; Code:

(require 'citeproc)
(require 'universal-sidecar)
(require 'universal-sidecar-citeproc)
(require 'org)
(require 'org-element)
(require 'oc)
(require 'ol)


;;; Get a filled processor for a buffer.

(defun org-cite-overlay-sidecar--get-processor (buffer &optional style)
  "Get a (filled) citation processor for BUFFER.

If a citation processor is not available from
`org-cite-overlay-mode', use STYLE if not nil, otherwise use the
default style from `universal-sidecar-citeproc-default-style'."
  (with-current-buffer buffer
    (if (and (fboundp 'org-cite-overlay-mode)
             (bound-and-true-p org-cite-overlay-mode)
             (bound-and-true-p org-cite-overlay-processor))
        org-cite-overlay-processor
      (when-let ((data-sources (mapcar #'expand-file-name (org-cite-list-bibliography-files)))
                 (references (org-element-map   (org-element-parse-buffer)
                                 'citation-reference
                               (lambda (citation)
                                 (org-element-property :key citation))))
                 (processor (universal-sidecar-citeproc-get-processor data-sources :style style)))
        (citeproc-add-uncited references processor)
        processor))))


;;; Define the Sidecar

(universal-sidecar-define-section org-cite-overlay-sidecar ((header "References") style)
                                  (:major-mode (org-mode))
  "Show citations from BUFFER in SIDECAR.

When `org-cite-overlay-mode' is active, and
`org-cite-overlay-processor' is non-nil, use that processor to
generate output, otherwise, generate output following
`universal-sidecar-citeproc' configuration.

Use a title of HEADER.  If using `universal-sidecar-citeproc',
use STYLE if provided."
  (when-let  ((processor (org-cite-overlay-sidecar--get-processor buffer style)))
    (with-current-buffer sidecar
      (universal-sidecar-insert-section org-cite-overlay-sidecar header
        (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                  (car (citeproc-render-bib processor 'org 'auto nil))
                  (save-match-data
                    (goto-char (point-min))
                    (while (re-search-forward org-target-regexp nil t)
                      (replace-match "")))))))))

(provide 'org-cite-overlay-sidecar)

;;; org-cite-overlay-sidecar.el ends here
