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

(eval-after-load 'phantomjs
  (assert (file-exists-p (expand-file-name phantomjs-home))))

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

(defconst phantomjs--base (file-name-directory (or (buffer-file-name)
                                                   load-file-name))
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
    (process-put proc :phantomjs-lock nil)
    (set-process-sentinel proc 'phantomjs--sentinel)
    (puthash name proc phantomjs--proc)
    proc))

(defcustom phantomjs-callback-debug t
  "Whether phantomjs web server callbacks should do debug messages or not.

The messages go to *Messages*."
  :group 'phantomjs
  :type '(boolean))

(defun phantomjs--callback (status args)
  (let (proc
        (http-status (save-match-data
                       (re-search-forward "HTTP/1.1 \\([0-9]+\\) .*" nil 't)
                       (match-string 1))))
    (when phantomjs-callback-debug
      (message "phantomjs--callback got status %s" http-status))
    (when (eq 200 (string-to-number http-status))
      (cond
       ((listp args)
        ;; The callback is for a specific user supplied callback.
        (setq proc (cadr args))
        (funcall (car args) http-status proc))
       ((processp args)
        (setq proc args))
       (t
        (when phantomjs-callback-debug
          (message "phantomjs--callback unknown callback type"))))
      ;; Unset the lock
      (when proc
        (when phantomjs-callback-debug
          (message "phantomjs--callback releasing the lock"))
        (process-put proc :phantomjs-lock nil)
        (when phantomjs-callback-debug
          (message "phantomjs--callback released the lock: %s %s"
                   (process-get proc :phantomjs-lock)
                   proc))))))

(defun phantomjs--wait-for (proc &optional no-grab)
  "Wait for lock on PROC to turn off, then immediately set it.

We set it on behalf of the caller, who is waiting to obtain the
lock.

With optional NO-GRAB don't take the lock."
  (while (process-get proc :phantomjs-lock)
    ;; (message "phantomjs--wait-for waiting for lock %s" proc)
    (sit-for 0.5))
  ;; (message "phantomjs--wait-lock released")
  (unless no-grab
    (process-put proc :phantomjs-lock t)))

(defun phantomjs-open (proc url &optional callback)
  "Open URL in PROCESS, the phantomjs instance.

If CALLBACK is specified and a function then, when the url is
opened call CALLBACK in the same way as with `url-retrieve'.

If the CALLBACK is not specified then attempt to wait on a signal
from the process."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "open")
           ("commandarg" . ,url))))
    (cond
     ((functionp callback)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list (list callback proc))))
     (t
      (phantomjs--wait-for proc)
      ;; (message "phantomjs-open got lock %s" proc)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list proc))))))

(defun phantomjs-call (proc javascript &optional callback)
  "Call JAVASCRIPT in PROCESS, the phantomjs instance.

When the Javascript completes call CALLBACK in the same way as
with `url-retrieve'."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "call")
           ("commandarg" . ,javascript))))
    (cond
     ((functionp callback)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list (list callback proc))))
     (t
      ;; (message "phantomjs-call waiting for lock %s" proc)
      (phantomjs--wait-for proc)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list proc))))))

(defun phantomjs-exit (proc &optional callback)
  "Make PROCESS, the phantomjs instance, exit.

When the exit is acknowledged call CALLBACK in the same way as
with `url-retrieve'."
  (assert (process-get proc :phantomjs-web-port))
  (let ((port (process-get proc :phantomjs-web-port))
        (url-request-extra-headers
         `(("command" . "exit"))))
    (cond
     ((functionp callback)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list (list callback proc))))
     (t
      ;; (message "phantomjs-exit waiting for lock %s" proc)
      (phantomjs--wait-for proc)
      (url-retrieve
       (format "http://localhost:%d" port)
       'phantomjs--callback
       (list proc))))))

(ert-deftest phantomjs-server ()
  "Test running a server.

This test is a little dangerous, there is a lot of waiting in it.
It may get stuck... you can always ctrl-G your way out of it."
  (let (finished
        blocking
        recorded-lock
        (server (phantomjs-server 'test 5100
                                  (lambda ()
                                    (setq finished t)))))
    (sleep-for 10)
    (should-not (process-get server :phantomjs-lock))
    (phantomjs-open
     server
     (concat "file://"
             (expand-file-name "test.html" phantomjs--base)))
    (should (process-get server :phantomjs-lock))
    ;; Do it the blocking way, this library does the blocking
    (phantomjs-call server "document.getElementById('title').id")
    (should (process-get server :phantomjs-lock))
    ;; Wait for the lock but don't take it
    (phantomjs--wait-for server t)
    (should-not (process-get server :phantomjs-lock))
    ;; A sequence of two blocking calls
    (phantomjs-call server "document.getElementById('title').id")
    (phantomjs-call server "document.getElementById('subtitle').id")
    ;; Wait for the lock but don't take it
    (phantomjs--wait-for server t)
    (should-not (process-get server :phantomjs-lock))
    (phantomjs-exit server)
    (should (process-get server :phantomjs-lock))
    (phantomjs--wait-for server t)
    (should-not (process-get server :phantomjs-lock))
    ;; we should test `finished' here
    ))

(provide 'phantomjs)

;;; phantom.el ends here
