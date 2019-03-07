;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)

(require 'org)
(org-babel-load-file
 (expand-file-name "~/.emacs.d/main.org"))

(put 'downcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
