;;;; versor-voice.el
;;; Time-stamp: <2004-01-23 16:43:42 john>
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

(provide 'versor-voice)
(require 'versor)

(defvar vr-versor-dimension-command-list nil
  "Vocal commands for versor")

(defun define-versor-vocal ()
  "Define the vocal commands."
  (interactive)
  (dolist (level (versor:all-level-names))
    (let* ((name (intern (concat "versor:select-" level)))
	   (body `(lambda ()
		    ,(format "Set the versor dimension to %s." level)
		    (interactive)
		    (versor:select-named-level ,level)))
	   )
      ;; (message "Defining %S to be %S" name body)
      (fset name body)
      (pushnew (cons level name) vr-versor-dimension-command-list :test 'equal))))

(if (null vr-versor-dimension-command-list) (define-versor-vocal))

(defvar vr-versor-command-list
  '(
    ("next" . versor:next)
    ("onwards" . versor:next)
    ("back" . versor:prev)
    ("out" . versor:out)
    ("in" . versor:in)
    ("first" . versor:start)
    ("initial" . versor:start)
    ("last" . versor:end)
    ("final" . versor:end)
    ("over" . versor:out-briefly)
    ("depth" . versor:select-in/out)
    ("expressions" . versor:select-expr)
    ("copy this" . versor:copy)
    ("cut this" . versor:kill)
    ("mark this" . versor:mark)
    ("valof" . wander-yank)
    ("result is" . pick-up-sexp-at-point)
    ("return" . exit-recursive-edit)
    )
  "Individually defined versor voice commands (and friends).")

;;; end of versor-voice.el
