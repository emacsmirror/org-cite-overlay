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


;;; Customization


;;; State Management

(defvar-local org-cite-overlay-processor nil
  "Overlay processor for the current buffer.")


;; Bibliograpy Management

;; From https://github.com/andras-simonyi/citeproc-el/issues/152#issuecomment-1881129883
;; I've looked into the package and it seems very interesting -- I'm
;; happy that you are making use of citeproc-el in it. As for adding a
;; defcustom, citeproc-el doesn't have customizable variables by
;; design, because it is intended to be a low-level library which is
;; not user-facing at all. If you want to delegate setting the CSL
;; locales (and styles) directory to another package I recommend using
;; the variables `org-cite-csl-locales-dir' and `org-cite-csl-styles-dir'
;; in oc-csl.el, which is part of Org and, therefore, Emacs. Relying
;; on Org in this respect could have the added benefit that it already
;; provides a locale-getter creating function, and Org actually
;; contains the default en_US CSL locale (and the Chicago author-date
;; CSL style).


;;; Overlay Management


;;; Minor Mode

(define-minor-mode org-cite-overlay-mode
  "A minor mode to show formatted org-cite citations using overlays.

Previews are disabled when cursor is within them, and re-enabled
when the cursor leaves."
  (if org-cite-overlay-mode
      (message "Enabled Case")
    (message "Disabled Case")))

(provide 'org-cite-overlay)

;;; org-cite-overlay.el ends here
