;;;; nested-blocks.el
;;; Time-stamp: <2006-02-15 16:10:50 jcgs>
;;
;; emacs-versor -- versatile cursors for GNUemacs
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

;; very pleasing -- lots of this worked first time

(provide 'nested-blocks)

(defvar nested-blocks-mode-starts
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\begin{\\([a-z]+\\)}\\)\\|{")
    (html-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
    (html-helper-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
    (c-mode . "[{(]")
    (java-mode . "[{(]")
    (perl-mode . "[{(]")
    (bcpl-mode . "\\$(")
    (t . "[[{(]"))
  "Alist showing how nested blocks start in each mode")

(defvar nested-blocks-mode-ends
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\end{\\([a-z]+\\)}\\)\\|}")
    (html-mode . "\\(</\\([^>]+\\)>\\)\\|\\()\\)\\|\\(''\\)")
    (html-helper-mode . "\\(</\\([^>]+\\)>\\)\\|\\()\\)\\|\\(''\\)")
    (c-mode . "[})]")
    (java-mode . "[})]")
    (perl-mode . "[})]")
    (bcpl-mode . "\\$)")
    (t . "[]})]"))
  "Alist showing how nested blocks finish in each mode")

(defvar nested-blocks-mode-starts-or-ends
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\\\(begin\\|end\\){\\([a-z]+\\)}\\)")
    (c-mode . "[({})]")
    (html-mode . "</?\\([^>]+\\)>")
    (html-helper-mode . "</?\\([^>]+\\)>")
    (bcpl-mode . "\\$[()]")
    (t . "[][}{)(]"))
  "Alist showing how nested blocks start or end in each mode.
This should match anything that either of the above match.
It's a bit clumsy for the programmer to have to declare it explicitly
but the performance should be a lot better than combining them
automatically, given the nature, for example, of HTML blocks.")

(defvar nested-blocks-mode-ignorables
  ;; todo: update to include TeX family and sh
  ;; todo: should have a way of saying that something is a start and an implicit end, like <li> in HTML and @section in LaTeX
  '((html-mode . "</?\\(li\\)\\|\\(d[dt]\\)\\|\\(br\\)\\|\\(img\\)")
    (html-helper-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\(br\\)\\|\\(img\\)\\)")
    (t .  ";"))
  "Alist showing things that look like nested block structure but are not.")

(defun nested-blocks-start ()
  "The starter for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-starts))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-starts)))))

(defun nested-blocks-end ()
  "The ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ends)))))

(defun nested-blocks-ignore ()
  "The ignorables for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ignorables))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ignorables)))))

(defun nested-blocks-start-or-end ()
  "The start-or-ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-starts-or-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-starts-or-ends)))))

(defun nested-blocks-enter ()
  "Into the next nested block forwards."
  (interactive)
  (re-search-forward (nested-blocks-start)))

(defun nested-blocks-leave ()
  "Move forwards out of current nested block."
  (interactive)
  (re-search-forward (nested-blocks-end)))

(defun nested-blocks-leave-backwards ()
  "Move backwards out of current nested block."
  (interactive)
  (re-search-backward (nested-blocks-start)))

(defun nested-blocks-another ()
  "Use the nested block behind point as a template to insert a new one."
  (interactive)
  (let ((template (save-excursion
		    (nested-blocks-backward)
		    (let ((start (point)))
		      (nested-blocks-forward)
		      (nested-blocks-template start (point))))))
    (apply 'insert template)))

(defun nested-blocks-forward ()
  "Move forward over a nested block. Like forward-sexp but more general."
  ;; ought to make this handle things in comments and strings
  (interactive)
  (let ((start (nested-blocks-start))
	(end (nested-blocks-end))
	(either (nested-blocks-start-or-end))
	(ignore (nested-blocks-ignore))
	(depth 0)
	)
    (message "trying to classify start=\"%s\" end=\"%s\" ignore=\"%s\"" start end ignore)
    (save-match-data
      (catch 'found
	(while (re-search-forward either (point-max) t)
	  (message "At \"%s\"" (match-string 0))
	  (save-excursion
	    (goto-char (match-beginning 0))
	    (cond
	     ((and ignore (looking-at ignore))
	      (message "ignoring")
	      t)
	     ((looking-at start)
	      (message "deeper")
	      (incf depth))
	     ((looking-at end)
	      (message "shallower")
	      (decf depth))
	     (t (error "inconsistency in nested-block patterns")))
	    (if (zerop depth)
		(throw 'found (point)))))
	nil))))

(defun nested-blocks-backward ()
  "Move back over a nested block. Like backward-sexp but more general."
  ;; ought to make this handle things in comments and strings
  (interactive)
  (let ((start (nested-blocks-start))
	(end (nested-blocks-end))
	(either (nested-blocks-start-or-end))
	(ignore (nested-blocks-ignore))
	(depth 0)
	)
    (save-match-data
      (catch 'found
	(while (re-search-backward either (point-min) t)
	  (save-excursion
	    (cond
	     ((and ignore (looking-at ignore)) t)
	     ((looking-at start) (decf depth))
	     ((looking-at end) (incf depth))
	     (t (error "inconsistency in nested-block patterns")))
	    (if (zerop depth)
		(throw 'found (point)))))))))

(defun nested-blocks-template (start end)
  "Make a nested block template from the buffer between START and END.
This is a list of strings."
  ;; todo: have the option of preserving the kind of white space around each template element, such as whether it was at the start of the line; see yank-whitespace.el for code for this
  (save-excursion
    (goto-char start)
    (let ((either (nested-blocks-start-or-end))
	  (template nil))
      (while (re-search-forward either end t)
	(push (buffer-substring-no-properties (match-beginning 0) (match-end 0))
	      template))
      (nreverse template))))

;; end of nested-blocks.el
