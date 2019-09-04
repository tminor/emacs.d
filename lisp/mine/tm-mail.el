;;; tm-mail.el --- Utils for mail in Emacs
;;
;; Copyright © Thomas Minor
;;
;; tm-mail.el is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; tm-mail.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with tm-mail.el.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for mail in Emacs; notmuch, message-mode, etc.

;;; Code:

(require 'notmuch)
(require 'all-the-icons)

;;;###autoload

(defun tm/notmuch-refresh-feed-buffer ()
  "Invoke `notmuch-refresh-this-buffer' specified buffer.

The buffer is silently refreshed, i.e. they are not forced to
be displayed."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (with-current-buffer buffer
        (when (and (memq buffer-mode '(notmuch-show-mode
                                       notmuch-tree-mode
                                       notmuch-search-mode
                                       notmuch-hello-mode))
                   (string= (buffer-name) "*notmuch-saved-search-feed*"))
          (notmuch-refresh-this-buffer))))))

(defun tm/set-feed-faces ()
  "Set `notmuch-search-line-faces' in feed buffer."
  (if (string= (buffer-name) "*notmuch-saved-search-feed*")
      (progn
        (setq notmuch-search-line-faces '(("git-commits" . '(:foreground "green"))
                                          ("nagios" . '(:foreground "red"))
                                          ("servicenow" . '(:foreground "yellow"))
                                          ("tenshi-uga" . '(:foreground "DodgerBlue1"))
                                          ("tenshi-db300" . '(:foreground "DodgerBlue1"))
                                          ("ssladmin" . '(:foreground "pink"))
                                          ("unread" . '(:background "gray15"))))
        (make-local-variable 'notmuch-search-line-faces))
    (setq notmuch-search-line-faces
          '(("unread" . notmuch-search-unread-face)
            ("flagged" . notmuch-search-flagged-face)))))

(defun tm/notmuch-notify ()
  "Generate desktop notifcations for new mail.

Utilize `notmuch-call-notmuch-sexp' to fetch the latest messages
tagged inbox and send a notification to the desktop"
  (let* ((latest-messages
          (apply
           #'notmuch-call-notmuch-sexp
           '("search"
             "--format=sexp"
             "--format-version=4"
             "--sort=newest-first"
             "tag:inbox"
             "date:45secs..")))
         (who)
         (when)
         (what)
         (mail-message)
         (body))
    (mapcar (lambda (mail-message)
              (setq when (plist-get mail-message :date_relative))
              (setq who (if (string-match-p "|"
                                            (plist-get mail-message :authors))
                            (progn (string-match "[[:space:],]\\{0,2\\}\\([a-zA-z[:space:]]+\\)|"
                                                 (plist-get mail-message
                                                            :authors))
                                   (match-string 1 (plist-get mail-message
                                                              :authors)))
                          (plist-get mail-message :authors)))
              (setq what (plist-get mail-message :subject))
              (setq body (format "<b>%s</b>\n<b>%s</b>\n\n%s" when who what))
              (notifications-notify :title "New message(s)!\n"
                                    :body body
                                    :app-name "notmuchmail"))
            latest-messages)))

(defun tm/notmuch-unread ()
  "Show unread message count in mode-line."
  (let* ((unread-mail (apply
                       #'notmuch-call-notmuch-sexp
                       '("search"
                         "--format=sexp"
                         "--format-version=4"
                         "--sort=newest-first"
                         "--output=messages"
                         "tag:inbox"
                         "and tag:unread"))))
    (setq unread-string (propertize
                         (format "%s %s"
                                 (all-the-icons-material
                                  "mail"
                                  :face 'all-the-icons-cyan-alt)
                                 (length unread-mail))
                         'font-lock-face '(:foreground "#61dafb")))
    (if (> (length unread-mail) 0)
        (progn
          (unless global-mode-string (setq global-mode-string '("")))
          (unless (memq unread-string global-mode-string)
            (setq global-mode-string (append global-mode-string
                                             '(unread-string)))))
      (setq global-mode-string nil))))

;; TODO: Add defgroup for notmuchfeed faces
(defface notmuch-feed-gluu
  '((t :foreground "purple2"
       :background "#6e6ed3d31110"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-nagios-problem
  '((t :foreground "#ffff00008080"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-nagios-recovery
  '((t :foreground "SpringGreen"
       :weight bold))
  "Highlight gluu messages.")
(defface notmuch-feed-inbox
  '((t :foreground "cyan"
       :weight bold))
  "Highlight gluu messages.")

(defun tm/highlight-notmuch-feed ()
  "Highlight interesting words in saved search."
  (let ((phrase-alist '(("idp-demo-prod" . notmuch-feed-gluu)
                        ("PROBLEM" . notmuch-feed-nagios-problem)
                        ("RECOVERY" . notmuch-feed-nagios-recovery)
                        ("inbox" . notmuch-feed-inbox))))
    (cl-loop for (key . value) in phrase-alist do
             (highlight-phrase key value))))

(provide 'tm-mail)
;;; tm-mail.el ends here
