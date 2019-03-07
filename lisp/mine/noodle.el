;;; noodle.el --- run noodle queries in emacs
;;
;; Copyright © Thomas Minor
;;
;; noodle.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; noodle.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with noodle.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Interact with noodle within Emacs.

;;; Code:

(defvar noodle-url "http://noodle.bor.usg.edu/"
  "Where to send noodle commands and queries.")

;;

(defun noodle-send-command (cmd query &optional list insert)
  "Send CMD and QUERY to noodle.  Return results as a list if LIST is non-nil.

Constructs a URl of the form
`http://noodle.bor.usg.edu/CMD/QUERY'.  Optionally, if LIST is
non-nil, return FQDNs in a list format for use as input to other
Lisp functions.  If INSERT is non-nil, insert the results of QUERY."
  (let ((url-request-method "GET")
	(arg-stuff (concat cmd "/" (url-hexify-string query))))
    (if list
	(with-temp-buffer
	  (url-insert-file-contents
	   (concat noodle-url arg-stuff))
	  (split-string (buffer-string)))
      (with-current-buffer
	  (switch-to-buffer
	   (get-buffer-create
	    (format "* noodle %s %s *" cmd query)))
	(url-insert-file-contents
	 (concat noodle-url arg-stuff))))))

(defun noodle-query (query)
  "Send QUERY to noodle and get some stuff back in a new buffer.

Essentially an interactive form of `noodle _ QUERY'.  When called
with prefix arg, insert results of QUERY at point."
  (interactive "sQuery: ")
  (if current-prefix-arg
      (insert (mapconcat 'identity (noodle-send-command "_" query t) "\n"))
    (noodle-send-command "_" query)))

(defun noodle-list (query)
  "Return a list of FQDNs returned for QUERY."
  (noodle-send-command "_" query t))

;;

(provide 'noodle)
;;; noodle.el ends here
