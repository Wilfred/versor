;;;; languide-edits.el
;;; Time-stamp: <2004-01-26 16:17:56 john>
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

(provide 'languide-edits)
(require 'cl)
(require 'languide)

(defun languide-unify-statements (n)
  "Make the next N statements into a single statement."
  (interactive "NNumber of statements to combine: ")
  (save-excursion
  (beginning-of-statement 1)
  (insert-compound-statement-open)
  (end-of-statement n)
  (insert-compound-statement-close)))

(defun languide-enclosing-scoping-point (n)
  "Move to the Nth-most closely enclosing scoping point.
If N is negative, potential scoping points are counted,
and the selected one is converted into a scoping point
if relevant.
For example, in Lisp, with a negative argument, \"progn\"
counts as a potential scoping point, and gets converted to
\"let ()\"."
  (interactive "NNumber of potential scoping levels to move out: ")
  )

(defun languide-enclosing-decision-point (n)
  "Move to the Nth-most closely enclosing decision point."
  (interactive "NNumber of decision levels to move out: ")
  )


(defmacro with-narrowing-to-scope (whereat &rest body)
  "With a restriction to the scope surrounding WHEREAT, execute BODY forms."
  `(save-restriction
     (destructuring-bind (start end)
	 (scope-around ,whereat)
       (narrow-to-region start end)
       ,@body)))


(defun employ-variable (whereat)
  "Take the text around point as a variable definition, and put it into use."
  (interactive "d")
  (destructuring-bind (name value
			    namestart nameend
			    valuestart valueend)
      (binding-around whereat)
    (with-narrowing-to-scope whereat
			     (save-excursion
			       (goto-char valueend)
			       (let ((ref (variable-reference name)))
				 (while (search-forward value (point-max) t)
				   (replace-match ref t t)))))))



;;; end of languide-edits.el
