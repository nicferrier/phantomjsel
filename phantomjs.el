;;; phantom.el --- handle phantomjs

;; Copyright (C) 2012  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Maintainer: Nic Ferrier <nferrier@ferrier.me.uk>
;; Created: 12 April 2012
;; Version: 0.0.3
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

(provide 'phantomjs)

;;; phantom.el ends here
