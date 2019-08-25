;;; tm-eshell.el --- Functions for Eshell
;;
;; Copyright © Thomas Minor
;;
;; tm-eshell.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tm-eshell.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tm-eshell.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for Eshell.

;;; Code:

(require 'eshell)
(require 'ivy)

(defun tm/setup-eshell-ivy-completion ()
  "Init Ivy completion for Eshell."
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  ;; only if you want to use the minibuffer for completions instead of
  ;; the in-buffer interface
  (setq-local ivy-display-functions-alist
              (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
                    ivy-display-functions-alist)))

;; Source: https://emacs.stackexchange.com/a/9521
;;
;; Example usage:
;;   ls "prefix-{A,B,C}.suffix"(|eshell-brace-expansion)
(defun eshell-brace-expansion (str)
  "Treat STR as an expandable brace expression, à la Bash."
  (let* ((parts (split-string str "[{}]"))
         (prefix (car parts))
         (body   (nth 1 parts))
         (suffix (nth 2 parts)))
    (mapcar (lambda (x) (concat prefix x suffix))
            (split-string body ","))))

(provide 'tm-eshell)
;;; tm-eshell.el ends here
