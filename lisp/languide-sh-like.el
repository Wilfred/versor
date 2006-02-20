;;;; languide-sh-like.el -- shell etc definitions for language-guided editing
;;; Time-stamp: <2004-11-01 11:49:40 john>
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

(provide 'languide-sh-like)

(defmodal binding-around sh-mode (whereat)
  "Return the shell variable definition around WHEREAT."
  (save-match-data
    (save-excursion
      (goto-char whereat)
      (beginning-of-line 1)
      (if (looking-at "^\\(export +\\)?\\([^=]+\\)=\\(.+\\)$")
	  (list (match-string 2)
		(match-string 3)
		(match-beginning 2) (match-end 2)
		(match-beginning 3) (match-end 3))
	nil))))

(defmodal scope-around sh-mode (whereat)
  "Return the scope (start and end) around WHEREAT."
  (save-excursion
    (let* ((start (safe-scan-lists whereat -1 1))
	   (end
	    (if start
		(progn
		  (goto-char start)
		  (forward-sexp 1)
		  (point))
	      (point-max))))
      (list (if start start (point-min))
	    end))))

(defmodal variable-reference sh-mode (varname)
  "make a variable reference from VARNAME"
  (concat "$" varname))
