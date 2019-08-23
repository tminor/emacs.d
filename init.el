(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; HACK: (require 'org) loads the version of org included with Emacs;
;; This interferes with loading packages that depend on org and can
;; cause problems. Instead, this calls `use-package' (and thereby
;; `straight-use-package' so that the most recent version of org is
;; loaded.
(use-package org
  :straight org-plus-contrib
  :demand t)

(org-babel-load-file
 (expand-file-name "~/.emacs.d/main.org"))

(put 'downcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
(put 'narrow-to-region 'disabled nil)
