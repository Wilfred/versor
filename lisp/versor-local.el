;;;; versor-local.el -- select navigation dimensions per mode or per buffer
;;; Time-stamp: <2007-07-17 12:04:50 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
;;
;; This file is part of emacs-versor.
;; 
;; emacs-versor is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;; 
;; emacs-versor is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with emacs-versor; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(require 'buffer-select-hooks)
(require 'cl)

;;;; our variables

(defvar versor-this-buffer-meta-level nil
  "The remembered meta-level for this buffer.")

(defvar versor-this-buffer-level nil
  "The remembered level for this buffer.")

(make-variable-buffer-local 'versor-this-buffer-meta-level)
(make-variable-buffer-local 'versor-this-buffer-level)

;;;; entry points

(defun versor-mode-change-function (old-mode)
  "Select dimension if necessary, as a hook on changing modes."
  (when (and versor-auto-change-for-modes
	     (not versor-per-buffer))
    ;; now get the meta-level and level for the new major mode
    (condition-case evar
	(let ((new-pair (assoc major-mode versor-mode-current-levels)))
	  (when (and new-pair
		     (numberp (cadr new-pair))
		     (numberp (cddr new-pair)))
	    (setq versor-meta-level (cadr new-pair)
		  versor-level (cddr new-pair))
	    ;; (message "Versor spotted mode change to %s, using %d %d" major-mode versor-meta-level versor-level)
	    (versor-set-status-display t)))
      (error nil))))

(defun versor-buffer-change-function (old-buffer)
  "Select dimension if necessary, as a hook on changing buffers."
  (when versor-per-buffer
    (condition-case evar
	(progn
	  (save-excursion
	    (set-buffer old-buffer)
	    (setq versor-this-buffer-meta-level versor-meta-level
		  versor-this-buffer-level versor-level))
	  (when (numberp versor-this-buffer-meta-level)
	    (setq versor-meta-level versor-this-buffer-meta-level)
	    (versor-trim-meta-level))
	  (when (numberp versor-this-buffer-level)
	    (setq versor-level versor-this-buffer-level)
	    (versor-trim-level))
	  (versor-set-status-display nil nil nil t))
      (error (message "error %S in versor-buffer-change-function" evar) 
	     nil))))

(defun versor-display-modal-levels (&optional label marked)
  "Display which levels are currently used for each mode."
  (interactive)
  (when (null marked) (setq marked (list major-mode)))
  (let* ((lengths (mapcar 'length
			  (mapcar (lambda (name)
				    (if (stringp name)
					name
				      (symbol-name name)))
				  (mapcar 'car
					  versor-mode-current-levels))))
	 (format-string (format "%%c %% %ds: %%s%%s:%%s%%s\n"
				(if (> (length lengths) 1)
				    (apply 'max lengths)
				  20))))
    (setq versor-mode-current-levels
	  (sort versor-mode-current-levels
		#'(lambda (a b)
		    (if (string= (car a) (car b))
			(stringp (car b))
		      (string< (car a) (car b))))))
    (with-output-to-temp-buffer (if label label "*Modal levels*")
      (dolist (level versor-mode-current-levels)
	(princ (format format-string
		       (if (member (car level) marked) ?* ? )
		       (car level)
		       (if (stringp (car level))
			   "\""
			 "<")
		       (aref (aref moves-moves (cadr level)) 0)
		       (first (aref (aref moves-moves (cadr level))
				    (cddr level)))
		       (if (stringp (car level))
			   "\""
			 ">"))))
      (princ (if versor-am-in-text-in-code
		 "\nIn text\n"
	       "In code\n"))
      (princ (format "post-command-hook=%S\n" post-command-hook))
      )))

(defun versor-popup-modal-levels (&optional label marked)
  "Briefly display the modal levels. Mostly for debugging."
  (save-window-excursion
    (sit-for 2)
    (versor-display-modal-levels label marked)
    (sit-for 4)))

;;;; setup

(add-hook 'mode-selection-hook 'versor-mode-change-function)

(add-hook 'buffer-selection-hook 'versor-buffer-change-function)

(provide 'versor-local)

;;;; end of versor-local.el

