;;; magit.el --- run magit queries in emacs
;;
;; Copyright © Thomas Minor
;;
;; magit.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; magit.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with magit.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for magit.

;;; Code:

(require 'magit)

;;;###autoload

;; https://github.com/dieggsy/dotfiles/blob/master/emacs.d/init.org#functions-7
(defun tm/magit-blame-toggle ()
  "Toggle magit-blame-mode on and off interactively.

Source: https://git.io/vQKub"
  (interactive)
  (if (bound-and-true-p magit-blame-mode)
      (magit-blame-quit)
    (call-interactively 'magit-blame)))

(provide 'tm-magit)
;;; tm-magit.el ends here
