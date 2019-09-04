;;; tm-color.el --- run tm-color queries in emacs
;;
;; Copyright © Thomas Minor
;;
;; tm-color.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tm-color.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tm-color.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utils for manipulating colors.

;;; Code:

(require 'color)

;;;###autoload

(defun tm/complement-for-color-at-point ()
  "Return color name for complement of color name at point."
  (interactive)
  (with-current-buffer (buffer-name)
    (let* ((complement-rgb-value
            (color-complement (thing-at-point 'word t)))
           (red (car complement-rgb-value))
           (blue (car (cdr complement-rgb-value)))
           (green (car (cdr (cdr complement-rgb-value))))
           (complement (color-rgb-to-hex red blue green))
           (color (thing-at-point 'word t)))
      (message "%s" (propertize complement
                                'face
                                `(:foreground ,(color-darken-name color 40)
                                  :background ,complement))))))

(provide 'tm-color)
;;; tm-color.el ends here
