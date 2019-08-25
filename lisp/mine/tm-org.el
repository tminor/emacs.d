;;; tm-org.el --- Utils and functions for `org-mode'
;;
;; Copyright © Thomas Minor
;;
;; tm-org.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tm-org.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tm-org.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Utilities and functions for `org-mode'.

;;; Code:

(defun tm/org-remove-inherited-local-tags ()
    "Remove local tags that can be inherited instead."
    (let* ((target-tags-local (org-get-tags-at nil 'local))
           ;; We have to remove the local tags otherwise they would not show up
           ;; as being inherited if they are present on parents---the local tag
           ;; would "override" the parent
           (target-tags-inherited
            (unwind-protect
                (progn
                  (org-set-tags-to nil)
                  (org-get-tags-at))
              (org-set-tags-to target-tags-local))))
      (-each target-tags-local
        (lambda (tag)
          (when (member tag target-tags-inherited)
            (org-toggle-tag tag 'off))))))

(provide 'tm-org)
;;; tm-org.el ends here
