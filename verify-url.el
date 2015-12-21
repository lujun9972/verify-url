;;; verify-url.el --- find out invalid urls in the buffer or region

;; Copyright (C) 2004-2015 Free Software Foundation, Inc.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2015-12-21
;; Version: 0.1
;; Keywords: convenience, usability, url

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Source code
;;
;; dmenu's code can be found here:
;;   http://github.com/lujun9972/verify-url

;;; Commentary:

;; verify-url is a little tool that used to find out invalid urls in the buffer or region

;; Quick start:

;; execute the following commands:
;; verify-url
;;

;;; Code:

(defgroup verify-url nil
  "verify url group")

(defcustom verify-url-regex
  "\\(file\\|ftp\\|http\\|https\\)://[^][:blank:]\r\n<>{}()*#$^['\\|]+"
  "regex that used to recognize urls")


(defcustom verify-url-time-out 10
  "expire time when connect to remote machine")

(defcustom verify-url-buffer "*verify-url*"
  "buffer to show the found invalid urls")

(defun verify-url--url-readable-p (url)
  (ignore-errors
    (save-match-data
      (with-timeout (verify-url-time-out nil)
        (let ((url-type (url-type (url-generic-parse-url url))))
          (cond ((equal url-type "ftp")
                 (url-ftp-file-readable-p url))
                ((equal url-type "file")
                 (url-file-file-readable-p url))
                ((equal url-type "http")
                 (url-http-file-readable-p url))
                ((equal url-type "https")
                 (url-https-file-readable-p url))
                (t
                 (file-readable-p url))))))))

;;;###autoload
(defun verify-url (&optional start end)
  "find out invalid urls in buffer or region"
  (interactive "r")
  (when (and (called-interactively-p 'any)
			 (not (use-region-p)))
	(setq start (point-min))
	(setq end (point-max)))
  (let (invalid-urls)
    (with-silent-modifications
      (save-excursion
        (goto-char start)
        (while (re-search-forward verify-url-regex end t)
          (let ((url (match-string 0 )))
            (unless (verify-url--url-readable-p url)
              (push url invalid-urls))))))
    (switch-to-buffer (get-buffer-create verify-url-buffer))
    (erase-buffer)
    (insert (format "invalid urls:\n%s" (string-join  invalid-urls "\n")))))

(provide 'verify-url)

;;; verify-url.el ends here
