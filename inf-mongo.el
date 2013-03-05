;;; inf-mongo.el --- Run a MongoDB shell process in a buffer

;; Copyright (C) 2013 Tobias Svensson

;; Author: Tobias Svensson
;; URL: http://github.com/tobiassvn/inf-mongo
;; Created: 1 March 2013
;; Keywords: databases mongodb
;; Version: 1.0.0

;; This file is released under the same terms as GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; inf-mongo.el provides a REPL buffer connected to a MongoDB shell
;; (mongo) subprocess.

;; Install

;; $ cd ~/.emacs.d/vendor
;; $ git clone git://github.com/tobiassvn/inf-mongo.git

;; In your emacs config:

;; (add-to-list 'load-path "~/.emacs.d/vendor/inf-mongo")
;; (require 'inf-mongo)

;; Usage

;; Run with `M-x inf-mongo'

;;; Code:

(require 'js)
(require 'comint)

;;;###autoload
(defgroup inf-mongo nil
  "Run a MongoDB shell (mongo) process in a buffer."
  :group 'inf-mongo)

;;;###autoload
(defcustom inf-mongo-command "/usr/local/bin/mongo 127.0.0.1:27017"
  "Default MongoDB shell command used.")

;;;###autoload
(defcustom inf-mongo-mode-hook nil
  "*Hook for customizing inf-mongo mode."
  :type 'hook
  :group 'inf-mongo)

(add-hook 'inf-mongo-mode-hook 'ansi-color-for-comint-mode-on)

;;;###autoload
(defun inf-mongo (cmd &optional dont-switch-p)
  "Major mode for interacting with an inferior MongoDB shell (mongo) process.

The following commands are available:
\\{inf-mongo-mode-map}

A MongoDB shell process can be fired up with M-x inf-mongo.

Customisation: Entry to this mode runs the hooks on comint-mode-hook and
inf-mongo-mode-hook (in that order)."
  (interactive (list (read-from-minibuffer "Run MongoDB shell: "
                                           inf-mongo-command)))

  (if (not (comint-check-proc "*mongo*"))
      (save-excursion (let ((cmdlist (split-string cmd)))
        (set-buffer (apply 'make-comint "mongo" (car cmdlist)
                           nil (cdr cmdlist)))
        (inf-mongo-mode))))
  (setq inf-mongo-command cmd)
  (setq inf-mongo-buffer "*mongo*")
  (if (not dont-switch-p)
      (pop-to-buffer "*mongo*")))

;;;###autoload
(defun mongo-send-region (start end)
  "Send the current region to the inferior MongoDB process."
  (interactive "r")
  (inf-mongo inf-mongo-command t)
  (comint-send-region inf-mongo-buffer start end)
  (comint-send-string inf-mongo-buffer "\n"))

;;;###autoload
(defun mongo-send-region-and-go (start end)
  "Send the current region to the inferior MongoDB process."
  (interactive "r")
  (inf-mongo inf-mongo-command t)
  (comint-send-region inf-mongo-buffer start end)
  (comint-send-string inf-mongo-buffer "\n")
  (switch-to-inf-mongo inf-mongo-buffer))

;;;###autoload
(defun mongo-send-last-sexp-and-go ()
  "Send the previous sexp to the inferior Mongo process."
  (interactive)
  (mongo-send-region-and-go (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun mongo-send-last-sexp ()
  "Send the previous sexp to the inferior MongoDB process."
  (interactive)
  (mongo-send-region (save-excursion (backward-sexp) (point)) (point)))

;;;###autoload
(defun mongo-send-buffer ()
  "Send the buffer to the inferior MongoDB process."
  (interactive)
  (mongo-send-region (point-min) (point-max)))

;;;###autoload
(defun mongo-send-buffer-and-go ()
  "Send the buffer to the inferior MongoDB process."
  (interactive)
  (mongo-send-region-and-go (point-min) (point-max)))

;;;###autoload
(defun switch-to-inf-mongo (eob-p)
  "Switch to the MongoDB process buffer.
With argument, position cursor at end of buffer."
  (interactive "P")
  (if (or (and inf-mongo-buffer (get-buffer inf-mongo-buffer))
          (mongo-interactively-start-process))
      (pop-to-buffer inf-mongo-buffer)
    (error "No current process buffer. See variable inf-mongo-buffer."))
  (when eob-p
    (push-mark)
    (goto-char (point-max))))

(defvar inf-mongo-buffer)

;;;###autoload
(defvar inf-mongo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-e" 'mongo-send-last-sexp)
    map))

;;;###autoload
(define-derived-mode inf-mongo-mode comint-mode "Inferior MongoDB mode"
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults (list js--font-lock-keywords))

  (make-local-variable 'syntax-propertize-function)
  (setq syntax-propertize-function #'js-syntax-propertize)

  (add-hook 'before-change-functions #'js--flush-caches t t)
  (js--update-quick-match-re)

  (use-local-map inf-mongo-mode-map))

(provide 'inf-mongo)
;;; inf-mongo.el ends here
