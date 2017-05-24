# Flycheck Clang Static Analyzer (scan-build) Checker

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/flycheck-clang-analyzer-badge.svg)](http://melpa.org/#/flycheck-clang-analyzer)
[![Build Status](https://travis-ci.org/alexmurray/flycheck-clang-analyzer.svg?branch=master)](https://travis-ci.org/alexmurray/flycheck-clang-analyzer)

Integrate [Clang Static Analyzer](https://clang-analyzer.llvm.org/)
(aka. scan-build) with [flycheck](http://www.flycheck.org) to automatically
perform on-the-fly static analysis *as you code*.

![flycheck-clang-analyzer screenshot](screenshots/flycheck-clang-analyzer.png)

This package leverages the configuration of the existing `c/c++-clang` flycheck
checker, or either [irony-mode](https://github.com/Sarcasm/irony-mode/)
or [rtags](https://github.com/Andersbakken/rtags) to provide the appropriate
compiler flags for clang - and so requires zero extra setup. This checker also
automatically chains itself as the next `flycheck` checker after
`c/c++-clang`, [flycheck-irony](https://github.com/Sarcasm/flycheck-irony/)
and [flycheck-rtags](https://github.com/Andersbakken/rtags) so that it only
runs when the corresponding previous checker returns without warnings. This
avoids trying to perform the analysis when there are syntactic errors etc.

## Installation

### MELPA (coming soon...)

The preferred way to install `flycheck-clang-analyzer` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
flycheck-clang-analyzer RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(with-eval-after-load 'flycheck
  (require 'flycheck-clang-analyzer)
  (flycheck-clang-analyzer-setup))
```

We recommend to use [use-package](https://github.com/jwiegley/use-package) to
make this automatic:

```emacs-lisp
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))
```

### Manual

If you would like to install the package manually, download or clone it and
place it within Emacs' `load-path`, then you can require it in your init file
like this:

```emacs-lisp
(require 'flycheck-clang-analyzer)
(flycheck-clang-analyzer-setup)
```

NOTE: This will also require the manual installation of `flycheck` if you have
not done so already.

## License

Copyright © 2017 Alex Murray

Distributed under GNU GPL, version 3.
