;;;; versor-language-edits.el -- versor commands to access commands in language-edits.el
;;; Time-stamp: <2006-02-10 10:07:56 jcgs>

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

(provide 'versor-language-edits)
(require 'languide-edits)

(defun versor:convert-selection-to-variable (name)
  "Make a variable declaration holding the current selection, and substitute it.
Useful when you realize you want to re-use a value you had calculated in-line.
The variable name is left at the top of the kill ring."
  (interactive "sVariable name: ")
  (versor:as-motion-command
   (let* ((item (versor:get-current-item)))
     (convert-region-to-variable (car item) (cdr item) name))))

(defun versor:convert-selection-to-function (name)
  "Take the selected code, and make it into a function, substituting a call to it.
The function name is left at the top of the kill ring."
  (interactive "sFunction name: ")
  (versor:as-motion-command
   (let* ((item (versor:get-current-item)))
     (convert-region-to-function (car item) (cdr item) name))))

(defun versor:surround-selection-with-call (name)
  "Surround the selection with a function call."
  (interactive "sFunction name: ")
  (versor:as-motion-command
   (let* ((item (versor:get-current-item)))
     (surround-region-with-call (car item) (cdr item) name))))

(defun versor:remove-function-call (name)
  "Remove the selected function call."
  (interactive)
  (versor:as-motion-command
   (let* ((item (versor:get-current-item)))
     (remove-surrounding-call (car item) (cdr item)))))

(defun versor:unify-statements ()
  "Unify statements"
  (interactive)
  (versor:as-motion-command
   (let* ((item (versor:get-current-item)))
     (languide-unify-statements-region (car item) (cdr item)))))

(defun versor:enclosing-scoping-point ()
  "Move to enclosing scoping point"
  (interactive)
  (versor:as-motion-command
   (languide-enclosing-scoping-point 1)))

(defun versor:enclosing-decision-point ()
  "Move to enclosing decision point"
  (interactive)
  (versor:as-motion-command
   (languide-enclosing-decision-point 1)))

(defun versor:employ-variable ()
  "Employ variable"
  (interactive)
  (versor:as-motion-command
   (languide-employ-variable (point))))

;;; end of versor-language-edits.el
