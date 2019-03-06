;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(package-initialize)

(setq package-enable-at-startup nil)

(setq gc-cons-threshold 400000000
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold (default-value 'gc-cons-threshold)
                   gc-cons-percentage (default-value 'gc-cons-percentage)
                   file-name-handler-alist (default-value 'file-name-handler-alist))))

(require 'org)
(org-babel-load-file
 (expand-file-name "~/.emacs.d/main.org"))

(put 'downcase-region 'disabled nil)
(put 'magit-clean 'disabled nil)
