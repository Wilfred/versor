;;;; languide-bindings.el -- handle variable bindings in a language-parameterized way
;;; Time-stamp: <2005-08-11 17:45:14 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'languide-bindings)
(require 'cl)
(require 'modal-functions)

(defmodel scope-around (whereat)
  "Return (as a list / multiple value) the start and end of the scope around WHEREAT.")

(defmodel binding-around (whereat)
  "Return (as a list / multiple value) a description of the binding around WHEREAT.
The results are:
  name                The name bound, as a string
  value               The expression giving the initial value for the variable,
                      as a string
  namestart nameend   Character positions of the start and end of the name
  valuestart valueend Character positions of the start and end of the initial
                      value expression
")

(defmodel variable-bindings-in-region (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string")

(defmodel variable-references-in-region (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location")

(defmodel static-variable-p (name where)
  "Return whether a static variable called NAME is visible at WHERE.")

(defun free-variables-in-region (from to)
  "Return a list of free variables between FROM and TO.
These are the variables used in that region, but not defined it it."
  (let ((references (variable-references-in-region from to))
	(bindings (variable-bindings-in-region from to))
	(result nil))
    (dolist (reference references)
      (let* ((name (car reference))
	     (binding (assoc name bindings)))
	(if binding
	    (let ((where-used (cadr reference)))
	      (unless (or (and (< (third binding) where-used)
			       (< where-used (fourth binding)))
			  (static-variable-p name from))
		(pushnew name result :test 'string=)))
	  (unless (static-variable-p name from)
	    (pushnew name result :test 'string=)))))
    (nreverse result)))

(defmodel variables-in-scope (whereat)
  "Return, as an alist, the names and types of variables in scope at WHEREAT.
Where types are not declared, as in Lisp, nil can be given as the type.")

(defmodel variable-reference (varname) "")

(defmodel move-to-enclosing-scope-last-variable-definition (&optional variables-needed)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible.")

(defmodel insert-variable-declaration (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
TYPE and INITIAL-VALUE may be null, but the NAME is required.")

;;; end of languide-bindings.el
