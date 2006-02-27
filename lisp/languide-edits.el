;;;; languide-edits.el
;;; Time-stamp: <2006-02-24 14:46:14 jcgs>
;;
;; Copyright (C) 2004, 2005, 2006  John C. G. Sturdy
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

(defun languide-unify-statements-region (a b)
  "Make the statements from A to B into a single statement."
  (interactive "r")
  (save-excursion
    (goto-char b)
    (insert-compound-statement-close)
    (goto-char a)
    (insert-compound-statement-open)))

(defun languide-enclosing-scoping-point (n)
  "Move to the Nth-most closely enclosing scoping point.
If N is negative, potential scoping points are counted,
and the selected one is converted into a scoping point
if relevant.
For example, in Lisp, with a negative argument, \"progn\"
counts as a potential scoping point, and gets converted to
\"let ()\"."
  (interactive "NNumber of potential scoping levels to move out: ")
  ;; todo: complete languide-enclosing-scoping-point
  )

(defun languide-enclosing-decision-point (n)
  "Move to the Nth-most closely enclosing decision point."
  (interactive "NNumber of decision levels to move out: ")
  ;; todo: complete languide-enclosing-decision-point
  )


(defmacro with-narrowing-to-scope (whereat &rest body)
  "With a restriction to the scope surrounding WHEREAT, execute BODY forms."
  `(save-restriction
     (destructuring-bind (start end)
	 (scope-around ,whereat)
       (narrow-to-region start end)
       ,@body)))


(defun languide-employ-variable (whereat)
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

(defun languide-convert-region-to-variable (from to name)
  "Take the region between FROM and TO, and make it into a variable called NAME."
  (interactive "r
sVariable name: ")
  (save-excursion
    (let ((value-text (buffer-substring-no-properties from to))
	  (variables-needed (free-variables-in-region from to)))
      (delete-region from to)
      (goto-char from)
      (insert name)
      (move-to-enclosing-scope-last-variable-definition variables-needed)
      (insert-variable-declaration name (deduce-expression-type value-text) value-text)
      (kill-new name))))

(defun languide-convert-region-to-function (begin end name &optional docstring)
  "Take the code between BEGIN and END, and make it into a function called NAME.
An optional DOCSTRING may also be given."
  (interactive "r
sFunction name: 
sDocumentation: ")
  (let* ((body-text (buffer-substring-no-properties begin end))
	 (arglist (free-variables-in-region begin end))
	 (result-type (deduce-expression-type body-text))
	 (begin-marker (make-marker))
	 )
    (delete-region begin end)
    (goto-char begin)
    (set-marker begin-marker begin)
    (insert-function-call name arglist)
    (move-before-defun)
    (insert-function-declaration name result-type arglist body-text docstring)
    (goto-char begin-marker)
    (kill-new name)))

(defun languide-surround-region-with-call (from to name)
  "Surround the region between FROM and TO with a call to NAME."
  (interactive "r
sFunction name: ")
  ;; todo: complete languide-surround-region-with-call
  )

(defun languide-remove-surrounding-call (from to)
  "Remove the function call around FROM and TO, leaving just the argument(s) to the function."
  (interactive "r")
  ;; todo: complete languide-remove-surrounding-call
  )

(defun languide-make-conditional ()
  ;; todo: write languide-make-conditional
  )

(defun languide-make-repeating ()
  ;; todo: write languide-make-repeating
)

(defun languide-remove-control ()
  ;; todo: write languide-remove-control
  )

;;; end of languide-edits.el
