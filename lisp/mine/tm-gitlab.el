;;; gitlab.el --- GitLab API
;;
;; Copyright © Thomas Minor
;;
;; gitlab.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; gitlab.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gitlab.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; GitLab API stuff.

;;; Code:

(require 'json)

(defvar gitlab-url "https://gitlab.usg.edu/api/v4")
(defvar gitlab-token "your_token_here")

;; Endpoints

(defvar gitlab-issue "/issues")
(defvar gitlab-issue-scope "?scope=")

;;

(defun gitlab-get (cmd &rest args)
  "Send API call to GitLab via CMD with ARGS."
  (let* ((url-request-method "GET")
	 (url-request-extra-headers
	  '(("PRIVATE-TOKEN" . "your_token_here"))))
    (with-current-buffer
	(switch-to-buffer (get-buffer-create "foo"))
      (url-insert-file-contents
       (concat gitlab-url
	       cmd
	       (string-join args "")))
      (let ((json-false :false))
	(json-read)))))

(provide 'gitlab)
;;; gitlab.el ends here
