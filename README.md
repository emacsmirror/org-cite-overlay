[![builds.sr.ht status](https://builds.sr.ht/~swflint/org-cite-overlay.svg)](https://builds.sr.ht/~swflint/org-cite-overlay?)

# `org-cite-overlay` [![MELPA](https://melpa.org/packages/org-cite-overlay-badge.svg)](https://melpa.org/#/org-cite-overlay)

Automatically toggle showing org-cite citations as formatted overlays upon cursor entry/exit.
To use, enable `org-cite-overlay-mode` in org mode buffers (for example, in `org-mode-hook`).
This code is heavily inspired by [`org-fragtog`](https://github.com/io12/org-fragtog/).

This package does have a few limitations:
 - Detection of exit or insertion of citations is somewhat inconsistent, so every 30 user commands in the buffer, all citations will be regenerated.
 - Only the default (chicago-author-year) style is used at the moment; this should eventually be fixed.

# `org-cite-overlay-sidecar` [![MELPA](https://melpa.org/packages/org-cite-overlay-sidecar-badge.svg)](https://melpa.org/#/org-cite-overlay-sidecar)

This is meant to replace [`org-cite-sidecar`](https://melpa.org/#/org-cite-sidecar), and will integrate with `org-cite-overlay`.
Similar to the former, it will show formatted org-cite citations in the sidecar, however, if `org-cite-overlay` is in use, the processor it fills will be used (reducing the amount of computation performed).
Because it is compatible with buffers not using `org-cite-overlay-mode`, it is also necessary to configure universal-sidecar-citeproc, as shown:

```elisp
(setq universal-sidecar-citeproc-locales "~/.emacs.d/csl-data/locales/"
      ;; set to your directories for locale and style data
      universal-sidecar-citeproc-styles "~/.emacs.d/csl-data/styles/")
(add-to-list 'universal-sidecar-sections 'org-cite-overlay-sidecar)
```

Additionally, there are two arguments to the section which are not
exposed as customization variables:

 - `:style` allows you to select a prefered CSL style within `universal-sidecar-citeproc-styles`.
 Default is `universal-sidecar-citeproc-default-style`.
 - `:header` allows you to change the header of the section from the default "References".

# Bug Reports and Patches

If you find an error or have a patch to improve this package, please send an email to `~swflint/emacs-utilities@lists.sr.ht`.
