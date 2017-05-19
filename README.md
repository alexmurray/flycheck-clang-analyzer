# Flycheck Clang Analyzer Checker

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/flycheck-clang-analyzer-badge.svg)](http://melpa.org/#/flycheck-clang-analyzer)
[![Build Status](https://travis-ci.org/alexmurray/flycheck-clang-analyzer.svg?branch=master)](https://travis-ci.org/alexmurray/flycheck-clang-analyzer)

Integrate [Clang Static Analyzer](https://clang-analyzer.llvm.org/)
with [flycheck](http://www.flycheck.org) to automatically check for defects in
your code on-the-fly.

This package depends on [irony-mode](https://github.com/Sarcasm/irony-mode/) to
provide the clang configuration so is intended for users of `irony-mode`.

## Installation

### MELPA (coming soon...)

The preferred way to install `flycheck-clang-analyzer` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
flycheck-clang-analyzer RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup)
  ;; chain after flycheck-irony in flycheck
  (flycheck-add-next-checker 'irony '(warning . clang-analyzer)))
```

### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'flycheck-clang-analyzer)
(flycheck-clang-analyzer-setup)
(flycheck-add-next-checker 'irony '(warning . clang-analyzer))
```

NOTE: This will also require the manual installation of `flycheck` and
`irony-mode` if you have not done so already.

## License

Copyright © 2017 Alex Murray

Distributed under GNU GPL, version 3.
