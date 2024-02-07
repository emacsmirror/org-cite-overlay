;;; org-cite-overlay-sidecar.el --- Show Sidecar for overlaid org-cite citations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Samuel W. Flint

;; Author: Samuel W. Flint <me@samuelwflint.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://git.sr.ht/~swflint/org-cite-overlay
;; Keywords: bib
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (citeproc "0.9.4") (org-cite-overlay "0.0.1") (universal-sidecar "1.5.0") (universal-sidecar-citeproc "1.0.0"))

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

(require 'citeproc)
(require 'universal-sidecar)
(require 'universal-sidecar-citeproc)
(require 'org)
(require 'org-element)
(require 'oc)
(require 'ol)


;;; Define the Sidecar

(universal-sidecar-define-section org-cite-overlay-sidecar ((header "References") style)
                                  (:major-mode (org-mode))
  "Show citations from BUFFER in SIDECAR.

When `org-cite-overlay-mode' is active, and
`org-cite-overlay-processor' is non-nil, use that processor to
generate output, otherwise, generate output following
`universal-sidecar-citeproc' configuration.

Use a title of HEADER.  If using `universal-sidecar-citeproc',
use STYLE if present."
  (if-let  ((processor (and (fboundp 'org-cite-overlay-mode)
                            (with-current-buffer buffer
                              (and org-cite-overlay-mode org-cite-overlay-processor)))))
      (with-current-buffer sidecar
        (universal-sidecar-insert-section org-cite-overlay-sidecar header
          (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                    (car (citeproc-render-bib processor 'org 'auto nil))
                    (save-match-data
                      (goto-char (point-min))
                      (while (re-search-forward org-target-regexp nil t)
                        (replace-match "")))))))
    (when-let* ((data-sources (mapcar #'expand-file-name (org-cite-list-bibliography-files)))
                (references (org-element-map (with-current-buffer buffer
                                               (org-element-parse-buffer))
                                'citation-reference
                              (lambda (citation)
                                (org-element-property :key citation))))
                (processor (universal-sidecar-citeproc-get-processor data-sources :style style)))
      (citeproc-add-uncited references processor)
      (with-current-buffer sidecar
        (universal-sidecar-insert-section org-cite-sidecar header
          (insert (universal-sidecar-fontify-as org-mode ((org-fold-core-style 'overlays))
                    (car (citeproc-render-bib processor 'org 'auto 'nil))
                    (save-match-data
                      (goto-char (point-min))
                      (while (re-search-forward org-target-regexp nil t)
                        (replace-match ""))))))))))

(provide 'org-cite-overlay-sidecar)

;;; org-cite-overlay-sidecar.el ends here
