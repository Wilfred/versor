;;;; languide-insertions.el
;;; Time-stamp: <2004-01-26 16:15:10 john>
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

(provide 'languide-insertions)
(require 'cl)
(require 'languide)

(defvar languide-insertion-sources
  '(languide-local-variables)
  "*List of functions to call to generate sources of possible things to insert.")

(defun languide-local-variables ()
  "Return the local variables in scope."
  '("foo" "bar" "baz" "i"))

(defun languide-possible-insertions ()
  "Generate the list of possible insertions."
  (let ((possibilities nil))
    (dolist (source languide-insertion-sources)
      (dolist (string (funcall source))
	(pushnew string possibilities)))
    (nreverse possibilities)))

(defun languide-insert ()
  "Do a language-guided insertion.
A bit like hippie-expand, but not using text already in the buffer at that point;
It offers completion on possibilities drawn from a variety of sources."
  (interactive)
  (let ((poss (languide-possible-insertions)))
))

(global-set-key [ insert ] 'languide-insert)
