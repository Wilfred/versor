;;;; modal-functions.el -- make a function which despatches on current major mode
;;; Time-stamp: <2004-03-02 10:01:36 john>
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

(provide 'modal-functions)

(defun safe-funcall (fn &rest args)
  "Call FN with remaining ARGS if not nil, else return first of ARGS"
  ;; (message "(safe-funcall %S %S)" fn args)
  (if fn
      (apply fn args)
    (car args)))

(defmacro defmodel (fun args doc &optional interactive)
  "Define a caller for contextual FUN with ARGS and optional INTERACTIVE.
The function is a property of that name on the symbol naming the major mode."
  (append
   (list 'defun fun args doc)
   (if interactive
       (list interactive)
     nil)
   (list (append (list 'safe-funcall
		       (list 'get 'major-mode (list 'quote fun)))

		 args))))


(defun defmodal0 (fun mode args body)
  "Define FUNCTION, for MODE with ARGS and BODY.
This is for use inside defmodal."
  (let* ((this-name (intern (concat (symbol-name mode) "-_-" (symbol-name fun)))))
    ;; (message "Defining %S to be %S for %S with args %S and body %S" this-name fun mode args body)
    (list 'progn
	  (append (list 'defun this-name args)
		  body)
	  (list 'put (list 'quote mode) (list 'quote fun) (list 'quote this-name)))))

(defmacro defmodal (fun mode args &rest body)
  "Define FUNCTION, for MODE with ARGS and BODY."
  (if (consp mode)
      (append
       '(progn)
       (mapcar (lambda (this-mode)
		 (defmodal0 fun this-mode args body))
	       mode))
    (defmodal0 fun mode args body)))
