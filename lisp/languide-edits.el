;;;; languide-edits.el
;;; Time-stamp: <2006-04-17 10:44:42 jcgs>
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

(defvar languide-auto-edit-overlays nil
  "Highlights added by languide to show what it has done.
Cleared at the start of each command.")

(defun languide-insertion (fn insertions)
  "Handler for things like insert, but remembering that the insertions were done by languide."
  (let ((start (point)))
    (apply fn insertions)
    (let* ((end (point))
	   (overlay (make-overlay start end)))
      (overlay-put overlay 'face 'languide-auto-edit-overlay-face)
      (push overlay languide-auto-edit-overlays))))

(defun languide-insert (&rest insertions)
  "Like insert, but remembers that the insertions were done by languide."
  (languide-insertion 'insert insertions))

(defun languide-insert-before-markers (&rest insertions)
  "Like insert-before-markers, but remembers that the insertions were done by languide."
  (languide-insertion 'insert-before-markers insertions))

(defun languide-remove-auto-edit-overlays ()
  "Remove any languide-auto-edit-overlays."
  (mapcar (lambda (o) (when (overlayp o) (delete-overlay o)))
	  languide-auto-edit-overlays)
  (setq languide-auto-edit-overlays nil))

(add-hook 'pre-command-hook 'languide-remove-auto-edit-overlays)

(defun languide-unify-statements (n)
  "Make the next N statements into a single statement."
  (interactive "NNumber of statements to combine: ")
  (save-excursion
    (beginning-of-statement 1)
    (let ((start (point)))
      (insert-compound-statement-open)
      (end-of-statement n)
      (insert-compound-statement-close)
      (indent-region start (point) nil))))

(defun languide-unify-statements-region (a b)
  "Make the statements from A to B into a single statement."
  (interactive "r")
  (save-excursion
    (let ((end-marker (make-marker)))
      (goto-char b)
      (insert-compound-statement-close)
      (set-marker end-marker (point))
      (goto-char a)
      (insert-compound-statement-open)
      (indent-region a end-marker nil))))

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
			    name-start name-end
			    value-start value-end)
      (binding-around whereat)
    (with-narrowing-to-scope whereat
			     (save-excursion
			       (goto-char value-end)
			       (let ((ref (variable-reference name)))
				 (while (search-forward value (point-max) t)
				   (replace-match ref t t)))))))


(defmodel adapt-binding-point ()
  "Make a binding point suitable for the binding that has just been added to it.")

(defmodel adjust-binding-point (variables-needed)
  "If appropriate, move to the first point at which all of VARIABLES-NEEDED are defined.
Assumes being at the end of a group of bindings, ready to insert a binding.")

(defun place-string (d) 
  (if (integerp d)
      (format "%d:\"%s\"" d (buffer-substring-no-properties d (+ 20 d)))
    (format "%S" d)))

(defun languide-convert-region-to-variable (from to name &optional nearest allow-conversions)
  "Take the expression between FROM and TO, and make it into a local variable called NAME.
NAME is left on the top of the kill ring, as this command is meant for when you realize
that you need to re-use the result of an expression.
With optional NEAREST, use the narrowest binding point; otherwise use the widest scope
in which all the variables used in the expression are defined.
With second optional ALLOW-CONVERSIONS, allow conversion of potential scoping points to
real ones (such as converting lisp's \"progn\" to \"let ()\")."
  (interactive "r
sVariable name: 
P")
  (when (string= name "") (setq name (symbol-name (gensym "foo_"))))
  (save-excursion
    (let ((value-text (buffer-substring-no-properties from to))
	  (variables-needed (free-variables-in-region from to)))
      (delete-region from to)
      (goto-char from)
      (insert name)
      
      (let ((binding-point (move-to-enclosing-scope-last-variable-definition allow-conversions)))
	;; see whether we can improve on the first binding point we find
	(when (and binding-point
		   (or (null nearest)
		       (integerp nearest)))
	  (let ((best-so-far (point)))
	    (message "best so far is now %d; binding-point is %S" best-so-far binding-point)
	    (while (and (progn
			  (goto-char binding-point)
			  (setq binding-point (move-to-enclosing-scope-last-variable-definition allow-conversions)))
			(all-variables-in-scope-p (point) variables-needed)
			(or (not (integerp nearest))
			    (not (zerop (setq nearest (1- nearest))))))
	      (setq best-so-far (point))
	      (message "best so far is now %d; binding-point is %S" best-so-far binding-point)
	      )
	    (when (null binding-point)
	      ;; if there's no binding point,
	      ;; move-to-enclosing-scope-last-variable-definition will
	      ;; have left point in the right place
	      (goto-char best-so-far)))))
      (adapt-binding-point)
      (adjust-binding-point variables-needed)
      (push (insert-variable-declaration name
					 (deduce-expression-type value-text from)
					 value-text)
	    languide-auto-edit-overlays)
      (kill-new name))))

(defun languide-convert-region-to-global (from to name)
  "Take the region between FROM and TO, and make it into a global variable called NAME."
  (interactive "r
sVariable name: ")
  (save-excursion
    (let ((value-text (buffer-substring-no-properties from to))
	  (variables-needed (free-variables-in-region from to)))
      (delete-region from to)
      (goto-char from)
      (insert name)
      (beginning-of-defun 1)
      (insert-global-variable-declaration name (deduce-expression-type value-text) value-text)
      (kill-new name))))

(defun languide-convert-region-to-function (begin end name &optional docstring)
  "Take the code between BEGIN and END, and make it into a function called NAME.
An optional DOCSTRING may also be given."
  (interactive "r
sFunction name: 
sDocumentation: ")
  (let* ((body-text (buffer-substring-no-properties begin end))
	 (arglist (free-variables-in-region begin end))
	 (result-type (deduce-expression-type body-text begin))
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
  (let ((arglist (list (buffer-substring-no-properties from to))))
    (delete-region from to)
    (insert-function-call name arglist)))

(defmodel languide-find-surrounding-call ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as
coming in any order, and being in any quantity; thus, if using them to
modify the buffer, it is usually necessary to sort them and deal with
them in descending order of character position.")

(defun versor-select-surrounding-call ()
  "Make the surrounding call into a versor selection."
  ;; mostly for debugging languide-find-surrounding-call
  (interactive)
  (versor-as-motion-command
   (versor-set-current-items (languide-find-surrounding-call))))

(defmacro those-rel-limit (those rel limit)
  `(let ((result nil)
	 (these ,those))
     (while these
       (when (,rel (car these) ,limit)
	 (setq result (cons (car these) result)))
       (setq these (cdr these)))
     (nreverse result)))

(defun those<=limit (those limit)
  "Return members of THOSE that are less than or equal to LIMIT."
  (those-rel-limit those <= limit))

(defun those>=limit (those limit)
  "Return members of THOSE that are greater than or equal to LIMIT."
  (those-rel-limit those >= limit))

(defun languide-remove-surrounding-call (&optional where)
  "Remove the function call around WHERE, leaving just the argument(s) to the function."
  (interactive "r")
  (save-excursion
    (when where (goto-char where))
    (let* ((call-syntax (sort (languide-find-surrounding-call)
			     ;; remove in descending order of
			     ;; position, as these are likely to be
			     ;; numbers rather than markers
			     (function
			      (lambda (a b)
				(> (car a) (car b))))))
	   (begins (mapcar 'car call-syntax))
	   (last-before (apply 'max (those<=limit begins where)))
	   (last-before-marker (make-marker))
	   (ends (mapcar 'cdr call-syntax))
	   (first-after (apply 'min (those>=limit ends where)))
	   (first-after-marker (make-marker)))
      (set-marker last-before-marker last-before)
      (set-marker first-after-marker first-after)
      (mapcar (function
	       (lambda (region)
		 ;; if this region is a symbol, remember it as probably
		 ;; being the function name
		 (when (save-excursion
			 (goto-char (car region))
			 (skip-syntax-forward "w_")
			 (>= (point) (cdr region)))
		   (kill-new (buffer-substring-no-properties
			      (car region) (cdr region))))
		 (delete-region (car region) (cdr region))))
	      call-syntax)
      (versor-trim-whitespace last-before-marker)
      (versor-trim-whitespace first-after-marker)
      (versor-set-current-item last-before-marker first-after-marker)
      (set-marker last-before-marker nil)
      (set-marker first-after-marker nil))))

(defun languide-make-conditional (from to condition)
  "Make the region between FROM and TO conditional upon CONDITION."
  (interactive "r
sCondition: ")
  (let ((body-type (languide-region-type from to)))
    (message "Body type from %d to %d is %s" from to body-type)
    (cond
     ((memq body-type '(compound-if-then-body if-then-body))
      (let ((navigate-container-whole-statement t))
	(navigate-this-container))
      (navigate-this-head)
      (let ((item (versor-get-current-item)))
	(add-expression-term 'and condition (car item) (cdr item))))
     ((memq body-type '(sequence t))
      (languide-unify-statements-region from to)
      (let ((inserter (cadar (get-statement-part 'if-then 'add-head)))
	    (tempo-insert-region t)
	    (old-marker (make-marker)))
	(message "inserter is %S" inserter)
	(goto-char from)
	(insert condition)
	;; we have to establish a region, for the template system to use,
	;; but we don't want the user to be able to see this, so do it in
	;; an underhand way
	(set-marker old-marker (marker-position (mark-marker)))
	(set-marker (mark-marker) from)
	(funcall inserter)
	(set-marker (mark-marker) old-marker)
	))
     (t (error "Not suitable for making conditional: %S" body-type))
     )))

(defun languide-make-iterative ()
  ;; todo: write languide-make-iterative -- try to use the existing skeleton or template
)

(defun languide-remove-control ()
  ;; todo: write languide-remove-control
  )

;;; end of languide-edits.el
