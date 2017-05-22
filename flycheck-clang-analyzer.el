;;; flycheck-clang-analyzer.el --- Integrate Clang Analyzer with flycheck

;; Copyright (c) 2017 Alex Murray

;; Author: Alex Murray <murray.alex@gmail.com>
;; Maintainer: Alex Murray <murray.alex@gmail.com>
;; URL: https://github.com/alexmurray/flycheck-clang-analyzer
;; Version: 0.1
;; Package-Requires: ((flycheck "0.24") (irony "1.0") (emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This packages integrates the Clang Analyzer `clang --analyze` tool with
;; flycheck to automatically detect any new defects in your code on the fly.
;;
;; It depends on and leverages `irony-mode' to allow the same clang
;; configuration (.clang_complete or compilation database etc.)
;;
;; Automatically chains itself as the next checker after `flycheck-irony'.

;;;; Setup

;; (with-eval-after-load 'flycheck
;;    (require 'flycheck-clang-analyzer)
;;    (flycheck-clang-analyzer-setup))

;;; Code:
(require 'flycheck)
(require 'irony)
(require 'irony-cdb)

(defun flycheck-clang-analyzer--get-compile-options ()
  "Get compile options for clang from irony."
  (nth 1 (irony-cdb--autodetect-compile-options)))

(defun flycheck-clang-analyzer--get-default-directory (_checker)
  "Get default directory for flycheck from irony."
  (nth 2 (irony-cdb--autodetect-compile-options)))

(flycheck-define-checker clang-analyzer
  "A checker using clang-analyzer.

See `https://github.com/alexmurray/clang-analyzer/'."
  :command ("clang"
	    "--analyze"
	    (eval (flycheck-clang-analyzer--get-compile-options))
	    ;; disable after compdb options to ensure stay disabled
	    "-fno-color-diagnostics" ; don't include color in output
	    "-fno-caret-diagnostics" ; don't indicate location in output
	    "-fno-diagnostics-show-option" ; don't show warning group
            source-inplace)
  :predicate (lambda () (irony-mode))
  :working-directory flycheck-clang-analyzer--get-default-directory
  :verify (lambda (_)
	    (list
	     (flycheck-verification-result-new
	      :label "Irony Mode"
	      :message (if irony-mode "enabled" "disabled")
	      :face (if irony-mode 'success '(bold error)))))
  :error-patterns ((warning line-start (file-name) ":" line ":" column ": warning: "
                            (message (one-or-more not-newline)
                                     (zero-or-more "\n"
                                                   (one-or-more space)
						   (one-or-more not-newline)))
                            line-end))
  :modes (c-mode c++-mode objc-mode))

;;;###autoload
(defun flycheck-clang-analyzer-setup ()
  "Setup flycheck-clang-analyzer.

Add `clang-analyzer' to `flycheck-checkers'."
  (interactive)
  ;; append to list and chain after existing checkers
  (add-to-list 'flycheck-checkers 'clang-analyzer t)
  (with-eval-after-load 'flycheck-irony
    (flycheck-add-next-checker 'irony '(warning . clang-analyzer))))

(provide 'flycheck-clang-analyzer)

;;; flycheck-clang-analyzer.el ends here
