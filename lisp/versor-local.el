;;;; versor-local.el -- select navigation dimensions per mode or per buffer
;;; Time-stamp: <2004-02-26 17:41:26 john>
;;
;; emacs-versor -- versatile cursors for GNUemacs
;;
;; Copyright (C) 2004  John C. G. Sturdy
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

(provide 'versor-local)
(require 'versor)
(require 'buffer-select-hooks)

(defvar versor:per-buffer nil
  "*Whether to remember the dimensions per buffer")

(defvar versor:this-buffer-meta-level nil
  "The remembered meta-level for this buffer.")

(defvar versor:this-buffer-level nil
  "The remembered level for this buffer.")

(make-variable-buffer-local 'versor:this-buffer-meta-level)
(make-variable-buffer-local 'versor:this-buffer-level)

(defvar versor:auto-change-for-modes t
  "*Whether to change the dimension on changing modes.")

(defvar versor:mode-current-levels nil
  "Alist of mode name symbols to the current meta-level and level for that mode.")

(defun versor:mode-change-function (old-mode)
  "Select dimension if necessary, as a hook on changing modes."
  (if (and versor:auto-change-for-modes
	   (not versor:per-buffer))
      (save-excursion
	(set-buffer old-buffer)
	(let ((old-pair (assoc major-mode versor:mode-current-levels)))
	  (if (null old-pair)
	      (push (cons major-mode (cons versor:meta-level versor:level))
		    versor:mode-current-levels)
	    (rplaca (cdr old-pair) versor:meta-level)
	    (rplacd (cdr old-pair) versor:level))))
    (let ((new-pair (assoc major-mode versor:mode-current-levels)))
      (when new-pair
	(setq versor:meta-level (cadr new-pair)
	      versor:level (cddr new-pair))
	(versor:set-status-display)))))

(defun versor:buffer-change-function (old-buffer)
  "Select dimension if necessary, as a hook on changing buffers."
  (if versor:per-buffer
      (progn
	(save-excursion
	  (set-buffer old-buffer)
	  (setq versor:this-buffer-meta-level versor:meta-level
		versor:this-buffer-level versor:level))
	(versor:select-named-meta-level versor:this-buffer-meta-level)
	(versor:select-named-level versor:this-buffer-level))))

(add-hook 'mode-selection-hook 'versor:mode-change-function)
(add-hook 'buffer-selection-hook 'versor:buffer-change-function)

