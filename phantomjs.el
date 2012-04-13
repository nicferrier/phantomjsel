;;; phantomjs.el --- handle phantomjs  -*- lexical-binding: t -*-

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 12 April 2012
;; Keywords: lisp

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

;;; Commentary:

;; This is code for running phantomjs as a child process of Emacs.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup phantomjs nil
  "Customizations for phantomjs handling."
  :group 'applications)

(defcustom phantomjs-home "~/phantomjs"
  "The location of phantomjs."
  :group 'phantomjs
  :type '(string))

(defvar phantomjs--proc
  (make-hash-table)
  "Hash of name->process object.")

(defun phantomjs--sentinel (proc status)
  "The sentinel for the phantomjs stuff.

Mostly exists for calling the end process callback."
  (cond
   ((member status '("finished\n"
                     "killed\n"
                     "exited abnormally with code 255\n"))
    (when (functionp (process-get proc :phantomjs-end-callback))
      (funcall (process-get proc :phantomjs-end-callback))))
   (t
    (message "phantom unexpected status: %s" status))))

;;;###autoload
(defun phantomjs-run (name callback &rest scripts)
  (interactive)
  "Run phantomjs process with NAME.

CALLBACK is called when it completes.

SCRIPTS is a list of scripts to be passed to the process."
  ;; need to check phantomjs--proc for name clash
  (let ((proc
         (apply
          'start-process
          (append
           (list
            (symbol-name name)
            (concat "* phantomjs-" (symbol-name name) " *")
            (expand-file-name (format "%s/bin/phantomjs" phantomjs-home)))
           (loop for script in scripts
                 collect (if (string-match "^http:.*" script)
                             script
                           (expand-file-name script)))))))
    (process-put proc :phantomjs-end-callback callback)
    (set-process-sentinel proc 'phantomjs--sentinel)
    (puthash name proc phantomjs--proc)))

(defconst phantomjs--base (file-name-directory load-file-name)
  "The base directory for the phantomjs elisp files.")

;;;###autoload
(defun phantomjs-server (name port complete-callback)
  (interactive)
  "Run a phantomjs process with NAME and a webserver on PORT.

COMPLETE-CALLBACK is called when it completes.

The webserver running on PORT is used to send commands to the
phantomjs instance using a special protocol.

Returns the process object representing the connection to
phantomjs."
  ;; need to check phantomjs--proc for name clash
  (let ((proc
         (start-process
          (symbol-name name)
          (concat "* phantomjs-" (symbol-name name) " *")
          (expand-file-name (format "%s/bin/phantomjs" phantomjs-home))
          (expand-file-name "ghostweb.js" phantomjs--base)
          (format "%d" port))))
    (process-put proc :phantomjs-end-callback complete-callback)
    (process-put proc :phantomjs-web-port port)
    (set-process-sentinel proc 'phantomjs--sentinel)
    (puthash name proc phantomjs--proc)
    proc))

(defun phantomjs--callback (status args)
  (let ((http-status (save-match-data
                       (re-search-forward "HTTP/1.1 \\([0-9]+\\) .*" nil 't)
                       (match-string 1))))
    (message "phantomjs callback got status %s" http-status)
    (when (eq 200 (string-to-number http-status))
      (funcall (car args) http-status (cadr args)))))

(defun phantomjs-open (proc url callback)
  "Open URL in PROCESS, the phantomjs instance.

When the url is opened call CALLBACK in the same way as with
`url-retrieve'."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "open")
           ("commandarg" . ,url))))
    (url-retrieve
     (format "http://localhost:%d" port)
     'phantomjs--callback
     (list (list callback proc)))))

(defun phantomjs-call (proc javascript callback)
  "Call JAVASCRIPT in PROCESS, the phantomjs instance.

When the Javascript completes call CALLBACK in the same way as
with `url-retrieve'."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "call")
           ("commandarg" . ,javascript))))
    (url-retrieve
     (format "http://localhost:%d" port)
     'phantomjs--callback
     (list (list callback proc)))))

(defun phantomjs-exit (proc callback)
  "Make PROCESS, the phantomjs instance, exit.

When the exit is acknowledged call CALLBACK in the same way as
with `url-retrieve'."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "exit"))))
    (url-retrieve
     (format "http://localhost:%d" port)
     'phantomjs--callback
     (list (list callback proc)))))

(provide 'phantomjs)

;;; phantom.el ends here
