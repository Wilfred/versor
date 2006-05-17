;;;; nested-blocks.el
;;; Time-stamp: <2006-05-10 20:27:13 jcgs>
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
;; less pleasing -- the rest was really fiddly

(provide 'nested-blocks)

(defvar nested-blocks-mode-starts
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\begin{\\([a-z]+\\)}\\)\\|{")
    (html-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
    (html-helper-mode . "\\(<\\([^!/>][^>]*\\)>\\)\\|\\([[({]\\)\\|\\(``\\)")
    (sgml-mode . "\\(<\\([^/>][^>]*\\)>\\)\\|\\((\\)\\|\\(``\\)")
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
    (html-helper-mode . "\\(</\\([^>]+\\)>\\)\\|\\([])}]\\)\\|\\(''\\)")
    (sgml-mode . "\\(</\\([^>]+\\)>\\)\\|\\()\\)\\|\\(''\\)")
    (c-mode . "[})]")
    (java-mode . "[})]")
    (perl-mode . "[})]")
    (bcpl-mode . "\\$)")
    (t . "[]})]"))
  "Alist showing how nested blocks finish in each mode")

(defvar nested-blocks-mode-comment-starts
  '((html-helper-mode . "<!--"))
  "Alist showing how comments start in each mode.")

(defvar nested-blocks-mode-comment-ends
  '((html-helper-mode . "-->"))
 "Alist showing how comments end in each mode.")

(defvar nested-blocks-mode-ignorables
  ;; todo: update to include TeX family and sh
  ;; todo: should have a way of saying that something is a start and an implicit end, like <li> in HTML and @section in LaTeX
  '((html-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\(br\\)\\|\\(img\\)\\(meta\\)\\)")
    (html-helper-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\([hb]r\\)\\|\\(img\\)\\|\\(meta\\)\\|\\(![a-z]\\)\\)")
    (sgml-mode . "</?\\(\\(li\\)\\|\\(d[dt]\\)\\|\\(br\\)\\|\\(img\\)\\)")
    (t .  ";"))
  "Alist showing things that look like nested block structure but are not.")

(defvar nested-blocks-mode-any
  ;; todo: update to include TeX family and sh
  '((latex-mode . "\\(\\\\\\(begin\\|end\\){\\([a-z]+\\)}\\)")
    (c-mode . "[({})]")
    (html-mode . "</?\\([^>]+\\)>")
    (html-helper-mode . "\\(<!--\\)\\|\\(</?\\([^>]+\\)>\\)\\|\\(-->\\)")
    (sgml-mode . "</?\\([^>]+\\)>")
    (bcpl-mode . "\\$[()]")
    (t . "[][}{)(]"))
  "Alist showing how nested blocks start or end in each mode.
This should match anything that either of the above match.
It's a bit clumsy for the programmer to have to declare it explicitly
but the performance should be a lot better than combining them
automatically, given the nature, for example, of HTML blocks.")

(defun nested-blocks-start ()
  "Alist of major mode to the starter for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-starts))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-starts)))))

(defun nested-blocks-end ()
  "Alist of major mode to the ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ends)))))

(defun nested-blocks-comment-start ()
  "Alist of major mode to the starter for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-comment-starts))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-comment-starts)))))

(defun nested-blocks-comment-end ()
  "Alist of major mode to the ender for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-comment-ends))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-comment-ends)))))

(defun nested-blocks-ignore ()
  "Alist of major mode to the \"ignorables\" regexp for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-ignorables))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-ignorables)))))

(defun nested-blocks-any ()
  "Alist of major mode to the start-or-ender-or-ignorable regexp for nested blocks in this mode.
A sensible default is provided if no mode-specific value is available."
  (let ((pattern (cdr (assoc major-mode nested-blocks-mode-any))))
    (if pattern
	pattern
      (cdr (assoc t nested-blocks-mode-any)))))

(defun nested-blocks-enter ()
  "Into the next nested block forwards."
  (interactive)
  ;; (re-search-forward (nested-blocks-start))
  (nested-blocks-forward 1 1)
  )

(defun nested-blocks-leave ()
  "Move forwards out of current nested block."
  (interactive)
  ;;(re-search-forward (nested-blocks-end))
  (nested-blocks-forward 1 -1)
  )

(defun nested-blocks-leave-backwards ()
  "Move backwards out of current nested block."
  (interactive)
  ;;(re-search-backward (nested-blocks-start))
  (nested-blocks-backward 1 -1)
  )

(defun nested-blocks-another ()
  "Use the nested block behind point as a template to insert a new one."
  (interactive)
  (let ((template (save-excursion
		    (nested-blocks-backward)
		    (let ((start (point)))
		      (nested-blocks-forward)
		      (nested-blocks-template start (point))))))
    (apply 'insert template)))

(defvar number-colours (vector "red" "pink" "orange" "yellow" "green"
			       "cyan" "sky blue" "deep sky blue" "dodger blue"
			       "purple" "white" "grey" "brown"))

(defvar nested-blocks-debug nil
  "*Whether to output messages while moving around nested blocks.")

(defvar nested-blocks-debug-show-structure nil
  "*Whether to fill in colours to show the block structure of the text.")

(defun nested-blocks-forward (&optional n d)
  "Move forward over a nested block. Like forward-sexp but more general.
Optional argument says how many blocks to move over.
Second argument says what level to stop at."
  ;; ought to make this handle things in comments and strings
  (interactive "p")
  (skip-to-actual-code)
  (let ((starting-point (point))
	(start (nested-blocks-start))
	(end (nested-blocks-end))
	(comment-start (nested-blocks-comment-start))
	(comment-end (nested-blocks-comment-end))
	(any (nested-blocks-any))
	(ignore (nested-blocks-ignore))
	(depth 0)
	(comment-depth 0)
	)
    (unless (numberp n) (setq n 1))
    (unless (numberp d) (setq d 0))
    (when nested-blocks-debug
      (message "trying to classify tags, using any=\"%s\" start=\"%s\" end=\"%s\" ignore=\"%s\"" any start end ignore))
    (while (> n 0)
      (catch 'found
	(while (re-search-forward any (point-max) t)
	  (save-match-data
	    (let ((starting (match-beginning 0))
		  (ending (match-end 0))
		  (old-depth depth))
	      (when nested-blocks-debug
		(message "At %d:\"%s\"" (point) (buffer-substring-no-properties starting ending)))
	      (save-excursion
		(goto-char (match-beginning 0))
		(cond
		 ((and comment-start (looking-at comment-start))
		  (setq comment-depth (1+ comment-depth))
		  (when nested-blocks-debug
		    (message "deeper comment: %d" comment-depth)))
		 ((and comment-end (looking-at comment-end))
		  (setq comment-depth (1- comment-depth))
		  (when nested-blocks-debug
		    (message "shallower comment: %d" comment-depth)))
		 ((and ignore (looking-at ignore))
		  (when nested-blocks-debug
		    (message "ignoring: %d" depth))
		  t)
		 ((looking-at start)
		  (when (zerop comment-depth)
		    (setq depth (1+ depth)))
		  (when nested-blocks-debug
		    (message "deeper: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 ((looking-at end)
		  (when (zerop comment-depth)
		    (setq depth (1- depth)))
		  (when nested-blocks-debug
		    (message "shallower: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 (t (error
		     "inconsistency in nested-block patterns: \"%s\" matched \"any\" but nothing specific"
		     (buffer-substring-no-properties starting ending))))
		(when nested-blocks-debug-show-structure
		  (put-text-property (if (< depth old-depth) ending starting) (point-max)
				     'face (cons 'background-color
						 (aref number-colours depth))))
		(when (= depth d)
		  (throw 'found (point))))))
	  nil))
      (setq n (1- n)))
    (versor-set-current-item starting-point (point))))

(defun nested-blocks-backward (&optional n d)
  "Move back over a nested block. Like backward-sexp but more general.
Optional argument says how many blocks to move over.
Second argument says what level to stop at."
  ;; ought to make this handle things in comments and strings
  (interactive "p")
  (let ((start (nested-blocks-start))
	(end (nested-blocks-end))
	(comment-start (nested-blocks-comment-start))
	(comment-end (nested-blocks-comment-end))
	(any (nested-blocks-any))
	(ignore (nested-blocks-ignore))
	(depth 0)
	(comment-depth 0)
	)
    (unless (numberp n) (setq n 1))
    (unless (numberp d) (setq d 0))
    (while (> n 0)
      (save-match-data
	(catch 'found
	  (while (re-search-backward any (point-min) t)
	    (let ((starting (match-beginning 0))
		  (ending (match-end 0)))
	      (when nested-blocks-debug
		(message "At %d:\"%s\"" (point) (buffer-substring-no-properties starting ending)))
	      (save-excursion
		(cond
		 ((and comment-end (looking-at comment-end))
		  (setq comment-depth (1+ comment-depth))
		  (when nested-blocks-debug
		    (message "deeper comment: %d" comment-depth)))
		 ((and comment-start (looking-at comment-start))
		  (setq comment-depth (1- comment-depth))
		  (when nested-blocks-debug
		    (message "shallower comment: %d" comment-depth)))
		 ((and ignore (looking-at ignore))
		  (when nested-blocks-debug
		    (message "ignoring: %d" depth))
		  t)
		 ((looking-at start)
		  (when (zerop comment-depth)
		    (setq depth (1- depth)))
		  (when nested-blocks-debug
		    (message "shallower: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 ((looking-at end)
		  (when (zerop comment-depth)
		    (setq depth (1+ depth)))
		  (when nested-blocks-debug
		    (message "deeper: %d%s" depth (if (zerop comment-depth) "" " but in comment"))))
		 (t (error
		     "inconsistency in nested-block patterns: \"%s\" matched \"any\" but nothing specific"
		     (buffer-substring-no-properties starting ending))))
		(if (= depth d)
		    (throw 'found (point))))))))
      (setq n (1- n)))))

(defun nested-blocks-template (start end)
  "Make a nested block template from the buffer between START and END.
This is a list of strings."
  ;; todo: have the option of preserving the kind of white space around each template element, such as whether it was at the start of the line; see yank-whitespace.el for code for this
  (save-excursion
    (goto-char start)
    (let ((any (nested-blocks-any))
	  (template nil))
      (while (re-search-forward any end t)
	(push (buffer-substring-no-properties (match-beginning 0) (match-end 0))
	      template))
      (nreverse template))))

;; end of nested-blocks.el
