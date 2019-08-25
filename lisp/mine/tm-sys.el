;;; sys.el --- run noodle queries in emacs
;;
;; Copyright © Thomas Minor
;;
;; sys.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; sys.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with sys.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; System utils such as `pgrep` implemented in Elisp only.

;;; Code:

(defun tm/pgrep (string)
  "Search process list for any process name matching STRING."
  (interactive "sProcess name: ")
  (let ((pid-list)
        (proc-match))
    ;; Gets a list of PIDs and constructs a list of lists containing
    ;; attribute alists for each process.
    (dolist (pid (list-system-processes) pid-list)
      (setq pid-list (cons (process-attributes pid) pid-list)))
    ;; Looks for STRING in PID-LIST and returns PID.
    (dolist (proc pid-list)
      (if (string-match-p (regexp-quote string)
                          (cdr (assoc-string 'args proc)))
          (return (cdr (assoc 'args proc)))
        nil))))

(provide 'tm/sys)
;;; sys.el ends here
