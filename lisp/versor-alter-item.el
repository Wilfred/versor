;;;; versor-alter-item.el -- choose possible value for the current item
;;; Time-stamp: <2005-02-11 10:48:10 john>
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

(provide 'versor-alter-item)
(require 'modal-functions)
(require 'languide)			; for variables-in-scope

(defun versor:alterations:set-current ()
  "Set the current alterations posssibility.
This writes it into the buffer and points versor's item at it, and updates the display."
  (let* ((old-item (versor:get-current-item))
	 (new-text (aref versor:alterations-values versor:alterations-index)))
    (delete-region (car old-item) (cdr old))
    (goto-char (car old-item))
    (let ((new-start (point)))
      (insert new-text)
      (versor:set-current-item new-start (point))
      (versor:alterations:show-current))))

(defun versor:alterations:get-type-values ()
  "Get the possible values for the current source of values, calculating them if necessary."
  (let* ((current-type (aref versor:alterations-types versor:alterations-type-index))
	 (current-type-name (car current-type))
	 (current-type-function (cadr current-type))
	 (current-type-values (cddr current-type)))
    (unless current-type-values
      (setq current-type-values (apply 'vector (funcall (cadr current-type-function) (point))))
      (rplacd (cdr current-type) current-type-values))
    current-type-values))

(defun versor:alterations:show-current ()
  "Show the current alterations posssibilities."
    (versor:alterations:get-type-values))

(defun versor:alterations-trim-index ()
  "Ensure that versor:alterations-type-index and versor:alterations-index are valid."
  (cond
   ((< 0 versor:alterations-type-index) (setq versor:alterations-type-index 0))
   ((> versor:alterations-type-index (1- (length versor:alterations-types)))))
  (setq versor:alterations-values (aref versor:alterations-types versor:alterations-type-index))
  (cond
   ((< 0 versor:alterations-index) (setq versor:alterations-index 0))
   ((> versor:alterations-index (1- (length versor:alterations-values))))))

(defun versor:alter-item-next ()
  "Replace the current item with the next possible value."
  (interactive)
  (versor:as-motion-command
   (incf versor:alterations-index)
   (versor:alterations-trim-index)
   (versor:alterations:set-current)))

(defun versor:alter-item-prev ()
  "Replace the current item with the previous possible value."
  (interactive)
  (versor:as-motion-command
   (decf versor:alterations-index)
   (versor:alterations-trim-index)
   (versor:alterations:set-current)))

(defun versor:alter-item-over-next ()
  "Replace the current item with the corresponding value from the next range."
  (interactive)
  (versor:as-motion-command
   (incf versor:alterations-type-index)
   (versor:alterations-trim-index)
   (versor:alterations:set-current)))

(defun versor:alter-item-over-prev ()
  "Replace the current item with the corresponding value from the previous range."
  (interactive)
  (versor:as-motion-command
   (decf versor:alterations-type-index)
   (versor:alterations-trim-index)
   (versor:alterations:set-current)))

(defmodel versor:get-alterations-possibility-types ()
  "Return an list of relevant alteration possibility types, relevant around point.
Each element is a list of a name and a function to call.
When called, that function should return a list of the possibilities of that type.
This gets cached in the cddr of the element.
A typical entry might be (\"Local variables\" variables-in-scope) where variables-in-scope
will be a function to list all the variables in scope at point.")

(defun tags-as-alist ()
  (save-excursion
    (let ((result nil))
      (visit-tags-table-buffer)
      (mapatoms (lambda (atom) (setq result (cons (list atom) result)))
		(tags-completion-table))
      result)))

(defun versor:alterations-get-current ()
  "Return, as a completion list, the current item.
Putting this into the possible types of alteration value is a way of making sure that the
current value is amongst the possible values."
  (let ((item (versor:get-current-item)))
    (list (buffer-substring (car item) (cdr item)))))

(defmodal versor:get-alterations-possibility-types (emacs-lisp-mode lisp-interaction-mode) ()
  "Return the possible types of values for emacs-lisp-mode."
  '(("local variables" 'variables-in-scope)
    ("current" 'versor:alterations-get-current)
    ("tags" 'tags-as-alist)))

(defvar versor:alterations-types nil
  "The possible types of alterations for the current alteration, as a vector.
The elements of the vector are the results of versor:get-alterations-possibility-types.")

(defvar versor:alterations-type-index 0
  "The current index into versor:alterations-types.")

(defvar versor:alterations-values nil
  "The possible values of alterations for the current type alteration, as a vector.
The elements of the vector are the results of an element of versor:get-alterations-possibility-types.")

(defvar versor:alterations-index 0
  "The current index into the current entry in versor:alterations-types.")

(defvar versor:alterations-old-keymap nil
  "The local map that was in use before we started alterations.")

(defvar versor:alterations-old-item nil
  "The old selection, just before we started altering it.")

(defvar versor:alterations-old-value nil
  "The value of the selected item just before we started altering it.")

(defun versor:begin-altering-item ()
  "Start selecting amonst possible values for the selected item.
Sets up mapping for the arrow keys, such that they now change the
value of the item, and the menu/select key to keep the value you
have at the time."
  (interactive)
  (versor:as-motion-command
   (setq versor:alterations-old-keymap (current-local-map)
	 versor:alterations-old-item (versor:get-current-item)
	 versor:alterations-old-value (buffer-substring
				       (car versor:alterations-old-item)
				       (cdr versor:alterations-old-item))
	 versor:alterations-types (apply 'vector (versor:get-alterations-possibility-types))
	 versor:alterations-type-index 0)
   (versor:alterations-trim-index)
   (use-local-map versor:altering-map)
   (catch 'done
     (while (< versor:alterations-type-index (length versor:alterations-types))
       (setq versor:alterations-index 0
	     versor:alterations-values (versor:alterations:get-type-values))
       (let ((n (length versor:alterations-values)))
	 (while (< versor:alterations-index n)
	   (if (string= versor:alterations-old-value
			(aref versor:alterations-values versor:alterations-index))
	       (throw 'done nil))
	   (incf versor:alterations-index)))
       (incf versor:alterations-type-index))
     (message "Could not find current"))
   (versor:alterations:show-current)))

(defun versor:end-altering-item ()
  "Take the currently selected value of the item, and quit alteration mode."
  (interactive)
  (versor:as-motion-command

   (setq versor:alterations-types nil)
   (use-local-map versor:alterations-old-keymap)))

;;; end of versor-alter-item.el
