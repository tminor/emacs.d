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

;; TODO: Move into files for `org-agenda', `org-refile', etc.

;;; Code:

(require 'org)
(require 'dash)
(require 'org-agenda)
(require 'autorevert)
(require 'cl-extra)
(require 'cl)

;;;###autoload

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

(defmacro tm/org-make-level-faces (level)
    "Generate commands and set vars for creating new `org-level-face's up to LEVEL."
    (let ((num 0))
      (while (< num (+ (string-to-number level) 1))
        (progn
          `(defface ,(intern (concat "org-level-"
                                     level))
             (org-compatible-face nil
               '((((class color) (min-colors 16) (background light))
                  (:foreground "RosyBrown"))
                 (((class color) (min-colors 16) (background dark))
                  (:foreground "LightSalmon"))
                 (((class color) (min-colors 8)) (:foreground "green"))))
             ,(format "Face used for level %s headlines." level)
             :group 'org-faces)
          (let ((face (intern (concat "org-level-" level))))
            (unless (member face
                            org-level-faces)
              (setq org-level-faces (append org-level-faces
                                            (list face))))))
        (setq num (+ num 1))))
    (setq org-n-level-faces
          (string-to-number level)))

;;; Defs for hashing and updating a heading's modification time.

(defun tm/getentryhash ()
  "Get the hash sum of the text in current entry, except :HASH:
and :MODIFIED: property texts."
  (save-excursion
    (let* ((full-str
            (buffer-substring-no-properties (point-min)
                                            (point-max)))
           (str-nohash
            (if (string-match "^ *:HASH:.+\n" full-str)
                (replace-match "" nil nil full-str)
              full-str))
           (str-nohash-nomod
            (if (string-match "^ *:MODIFIED:.+\n" str-nohash)
                (replace-match "" nil nil str-nohash)
              str-nohash))
           (str-nohash-nomod-nopropbeg
            (if (string-match "^ *:PROPERTIES:\n" str-nohash-nomod)
                (replace-match "" nil nil str-nohash-nomod)
              str-nohash-nomod))
           (str-nohash-nomod-nopropbeg-end
            (if (string-match "^ *:END:\n" str-nohash-nomod-nopropbeg)
                (replace-match "" nil nil str-nohash-nomod-nopropbeg)
              str-nohash-nomod-nopropbeg)))
      (secure-hash 'md5 str-nohash-nomod-nopropbeg-end))))

(defun tm/update-modification-time ()
  "Set the :MODIFIED: property of the current entry to NOW and
update :HASH: property."
  (save-excursion
    (save-restriction
      (let* ((beg
              (progn
                (org-back-to-heading)
                (point)))
             (end
              (progn
                (outline-next-heading)
                (- (point) 1))))
        (narrow-to-region beg end)
        (org-set-property "HASH"
                          (format "%s" (tm/getentryhash)))
        (org-set-property "MODIFIED"
                          (format-time-string "[%Y-%m-%d %a %H:%M]"))))))

(defun tm/skip-nonmodified ()
  "Skip headings whose :MODIFIED: properties are unchanged."
  (let ((next-headline
         (save-excursion
           (or (outline-next-heading)
               (point-max)))))
    (save-restriction
      (let* ((beg
              (progn
                (org-back-to-heading)
                (point)))
             (end
              (progn
                (outline-next-heading)
                (- (point) 1))))
        (narrow-to-region beg end)
        (if (string= (org-entry-get (point) "HASH" nil)
                     (format "%s" (tm/getentryhash)))
            next-headline
          nil)))))

;; Functions for agenda navigation.
;; TODO: Debug functions.
;;
;; Source: https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html

(defun tm/org-agenda-next-header ()
  "Jump to the next header in an agenda series."
  (interactive)
  (tm/org-agenda-goto-header))

(defun tm/org-agenda-previous-header ()
  "Jump to the previous header in an agenda series."
  (interactive)
  (tm/org-agenda-goto-header t))

(defun tm/org-agenda-goto-header (&optional backwards)
  "Find the next agenda series header forwards or BACKWARDS."
  (let ((pos (save-excursion
               (goto-char (if backwards
                              (line-beginning-position)
                            (line-end-position)))
               (let* ((find-func (if backwards
                                     'previous-single-property-change
                                   'next-single-property-change))
                      (end-func (if backwards
                                    'max
                                  'min))
                      (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                         (funcall find-func (point) 'org-agenda-date-header)))
                      (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                      (prop-pos (if all-pos (apply end-func all-pos) nil)))
                 prop-pos))))
    (if pos (goto-char pos))
    (if backwards (goto-char (line-beginning-position)))))

;; Defs for reverting buffers automatically if they change on disk.
;;
;; Source: https://stackoverflow.com/a/13946304

(defvar tm/auto-minor-mode-alist ()
  "Alist of filename patterns vs correpsonding minor mode functions.

See `auto-mode-alist' All elements of this alist are
checked, meaning you can enable multiple minor modes for the same
regexp.")

(defun tm/enable-minor-mode-based-on-extension ()
  "Check file name against `tm/auto-minor-mode-alist' to enable minor modes.
The checking happens for all pairs in tm/auto-minor-mode-alist"
  (when buffer-file-name
    (let ((name (file-name-sans-versions buffer-file-name))
          (remote-id (file-remote-p buffer-file-name))
          (case-fold-search auto-mode-case-fold)
          (alist tm/auto-minor-mode-alist))
      ;; Remove remote file name identification.
      (when (and (stringp remote-id)
                 (string-match-p (regexp-quote remote-id) name))
        (setq name (substring name (match-end 0))))
      (while (and alist (caar alist) (cdar alist))
        (if (string-match-p (caar alist) name)
            (funcall (cdar alist) 1))
        (setq alist (cdr alist))))))

(setq auto-revert-verbose nil)

;; Source: https://emacs.stackexchange.com/a/26369
(defun tm/org-cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by PROP.

If a is before b, return -1. If a is after b, return 1. If they
are equal return t."
  (lexical-let ((prop prop))
    #'(lambda (a b)
	(let* ((a-pos (get-text-property 0 'org-marker a))
	       (b-pos (get-text-property 0 'org-marker b))
	       (a-date (or (org-entry-get a-pos prop)
			   (format "<%s>" (org-read-date t nil "now"))))
	       (b-date (or (org-entry-get b-pos prop)
			   (format "<%s>" (org-read-date t nil "now"))))
	       (cmp (compare-strings a-date nil nil b-date nil nil)))
	  (if (eq cmp t) nil (signum cmp))))))

;; Defs for `org-capture'

(defun transform-square-brackets-to-round-ones (string-to-transform)
      "Transforms [ into ( and ] into ), other chars left unchanged."
      (concat
       (mapcar #'(lambda (c)
                   (if (equal c ?[) ?\( (if (equal c ?]) ?\) c)))
               string-to-transform)))

(defmacro tm/org-get-headings-command (fn-suffix target)
  "Generate a command for capturing to TARGET."
  `(defun ,(intern (concat "tm/org-get-headings-"
			   (symbol-name fn-suffix))) ()
     ,(format "Return `point' for heading in %S" target)
     (interactive)
     (let* ((file (concat (if (string= ,target
				       "main.org")
			      user-emacs-directory
			    org-base-directory)
			  ,target))
	    (buf (find-buffer-visiting file)))
       (unless buf
	 (find-file file))
       (with-current-buffer
	   buf
	 ;; Gets headings from TARGET and fontifies them before collecting
	 ;; them in `heading-point-alist', each cons cell of which reprents a
	 ;; heading (with text properties) pointing at the value for that
	 ;; heading's point.  `heading-point-alist' is passed to
	 ;; `completing-read' read, ultimately calling `goto-char' against the
	 ;; point from the chosen cons cell.
	 (let* ((heading-point-alist '())
		(headings
		 (org-map-entries
		  (lambda ()
		    (cl-pushnew `(,(save-excursion
				     (org-format-outline-path
				      (org-get-outline-path t)))
				  . ,(goto-char (point)))
				heading-point-alist
				:test #'equal)))))
	   (goto-char (cdr (assoc
			    (completing-read "File under: "
					     heading-point-alist)
			    heading-point-alist))))))))

;; Adds functions, advice, etc. for killing a new frame if one has
;; been created by org-capture browser extension.
;;
;; https://github.com/sprig/org-capture-extension#example-closins-the-frame-after-a-capture
(defvar tm/delete-frame-after-capture 0
  "Whether to delete the last frame after the current capture.")

;; TODO: Is this needed?
(defun tm/delete-frame-if-neccessary (&rest r)
  (cond
   ((= tm/delete-frame-after-capture 0) nil)
   ((> tm/delete-frame-after-capture 1)
    (setq tm/delete-frame-after-capture (- tm/delete-frame-after-capture 1)))
   (t
    (setq tm/delete-frame-after-capture 0)
    (delete-frame))))
(advice-add 'org-capture-finalize
            :after 'tm/delete-frame-if-neccessary)
(advice-add 'org-capture-kill
            :after 'tm/delete-frame-if-neccessary)
(advice-add 'org-capture-refile
            :after 'tm/delete-frame-if-neccessary)

(provide 'tm-org)
;;; tm-org.el ends here
