;;; versor-base-moves.el -- versatile cursor
;;; Time-stamp: <2006-05-18 11:06:36 jcgs>
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

(require 'cl)
(require 'modal-functions)
(provide 'versor-base-moves)

;; some internal functions for operations that aren't typically there already,
;; or that normal emacs does slightly differently from what we want

(defun versor-forward-paragraph (&optional n)
  "Like forward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (forward-paragraph n)
  (message "forward-paragraph %d; then forward over blank lines" n)
  (if (> n 0)
      (while (looking-at "^$")
	(message "skipping blank line at start of paragraph")
	(forward-line 1))))

(defun versor-backward-paragraph (&optional n)
  "Like backward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (backward-paragraph (1+ n))
  (message "backward-paragraph %d; then forward over blank lines" n)
  (if (> n 0)
      (while (looking-at "^$")
	(message "skipping blank line at start of paragraph")
	(forward-line 1))))

(defun versor-end-of-paragraph (&optional arg)
  "Move to the end of the paragraph."
  (end-of-paragraph-text))

;; todo: add tex, latex, bibtex ideas of paragraphs, perhaps going via generic-text, or building on the built-in ones in-place?

(defun versor-start-of-line (n)
  "Move to first non-space on line, or to start of Nth line from here."
  (interactive "p")
  (if (or (= n 1)
	  (not (eq last-command 'versor-start-of-line)))
      (back-to-indentation)
    (beginning-of-line n)))

(defmacro versor-following-margins (&rest body)
  "Perform BODY while sticking to the same margin if on one."
  `(let* ((at-left-edge (bolp))
	  (at-right-margin (eolp))
	  (at-left-margin
	   (or				; don't calculate this if either of the others hold
	    at-right-margin		; as it is slower, and is not used if the others are
	    at-left-edge
	    (save-excursion (skip-syntax-backward "\\s-") (bolp)))))
     ,@body
     (cond
      (at-left-edge (beginning-of-line 1))
      (at-right-margin (end-of-line 1))
      (at-left-margin (back-to-indentation)))))

(defun versor-previous-line (n)
  "Move to the previous line, following margins if on one."
  (interactive "p")
  (versor-following-margins (previous-line n)))

(defun versor-next-line (n)
  "Move to the next line, following margins if on one."
  (interactive "p")
  (versor-following-margins (next-line n)))

(defun safe-up-list (&rest args)
  "Like up-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'up-list args)
	t)
    (error nil)))

(defun safe-backward-up-list (&rest args)
  "Like backward-up-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'backward-up-list args)
	t)
    (error nil)))

(defun safe-backward-sexp (&rest args)
  "Like backward-sexp, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'backward-sexp args)
	t)
    (error nil)))

(defun safe-down-list (&rest args)
  "Like down-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'down-list args)
	t)
    (error nil)))

(defun safe-scan-sexps (from count)
  "Like scan-sexps, but returns nil on error."
  (condition-case error-var
      (scan-sexps from count)
    (error nil)))

(defun safe-scan-lists (from count depth)
  "Like scan-lists, but returns nil on error."
  (condition-case
      error-var
      (scan-lists from count depth)
    (error nil)))

(defun safe-forward-sexp (from)
  "Like forward-sexp, but returns point on success and nil on error."
  (condition-case error-var
      (progn
	(forward-sexp from)
	(point))
    (error nil)))

(defmodel versor-backward-up-list (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p"))

(defmodal versor-backward-up-list (fundamental-mode) (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p")
  (safe-backward-up-list arg)
  (when versor-reformat-automatically
    (save-excursion
      (condition-case evar
	  (let ((start (point)))
	    (forward-sexp 1)
	    (indent-region start (point) nil))
	(error nil))))
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (let ((start (point))
	(end (save-excursion (forward-sexp 1) (point))))
    ;; TODO: should probably be versor-set-current-item
    (make-versor-overlay start (1+ start))
    ;; (message "extra overlay at %d..%d" (1- (point)) (point))
    ;; TODO: should probably be versor-add-to-current-item when I've written one
    (versor-extra-overlay (1- end) end)
    (cons start end)))

(defmodal versor-backward-up-list (lisp-mode emacs-lisp-mode lisp-interaction-mode) (arg)
  "Like backward-up-list, but with some versor stuff around it.
Makes a two-part selection, of opening and closing brackets."
  (interactive "p")
  (safe-backward-up-list arg)
  (when (and versor-reformat-automatically
	     (buffer-modified-p))
    (condition-case evar
	(indent-sexp)
      ;; todo: use (indent-region start end nil) instead
      (error nil)))
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (let ((start (point))
	(end (save-excursion (forward-sexp 1) (point))))
    ;; TODO: should probably be versor-set-current-item
    (make-versor-overlay start (1+ start))
    ;; (message "extra overlay at %d..%d" (1- (point)) (point))
    ;; TODO: should probably be versor-add-to-current-item when I've written one
    (versor-extra-overlay (1- end) end)
    (cons start end)))

(defmodal versor-backward-up-list (html-mode html-helper-mode) (arg)
  "Like backward-up-list, but with versor stuff around it, and for HTML blocks."
  (nested-blocks-leave-backwards)
  (let ((open-start (match-beginning 0))
	(open-end (match-end 0)))
    (nested-blocks-forward)
    (let ((close-start (match-beginning 0))
	  (close-end (match-end 0)))
      (make-versor-overlay open-start open-end)
      (versor-extra-overlay close-start close-end)
      (goto-char open-start))))

(defmodel versor-down-list (arg)
  "Like down-list, but with some versor stuff around it."
  (interactive "p"))

(defmodal versor-down-list (fundamental-mode) (arg)
  "Like down-list, but with some versor stuff around it."
  (interactive "p")
  (unless (safe-down-list arg)
    ;; if we were at the end of the last expression, try going to the start of it
    (previous-sexp 1)
    (safe-down-list args))
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (make-versor-overlay (point) (1+ (point))) ; TODO: should probably be versor-set-current-item
  (when (looking-at "\\s(")
    (save-excursion
      (forward-sexp 1)
      ;; (message "extra overlay at %d..%d" (1- (point)) (point))
      (versor-extra-overlay (1- (point)) (point))))) ; TODO: should probably be versor-add-to-current-item when I've written one

(defmodal versor-down-list (html-mode html-helper-mode) (arg)
  "Like down-list, but with versor stuff around it, and for HTML block structure."
  (nested-blocks-enter))

;; left over from trying a window selection dimension
;; (defun other-window-backwards (n)
;;   "Select the -Nth window -- see other-window, this just negates the argument."
;;   (interactive "p")
;;   (other-window (- n)))

;; (defun first-window ()
;;   "Select the first window in the frame"
;;   (interactive)
;;   (select-window (window-at 0 0)))

;; (defun last-window ()
;;   "Select the last window in the frame"
;;   (interactive)
;;   (select-window (window-at (1- (frame-width)) (- (frame-height) 3))))

;; left over from a bumping character values dimension
;; (defun zero-char ()
;;   "Make the character at point be zero."
;;   (save-excursion
;;     (delete-region (point) (1+ (point)))
;;     (insert 0)))

;; (defun dec-char ()
;;   "Decrement the character at point."
;;   (interactive)
;;   (save-excursion
;;     (let ((new-char (1- (char-after (point)))))
;;       (delete-region (point) (1+ (point)))
;;       (insert new-char))))

;; (defun inc-char ()
;;   "Increment the character at point."
;;   (interactive)
;;   (save-excursion
;;     (let ((new-char (1+ (char-after (point)))))
;;       (delete-region (point) (1+ (point)))
;;       (insert new-char))))

;; (defun max-char ()
;;   "Make the character at point be zero."
;;   (save-excursion
;;     (delete-region (point) (1+ (point)))
;;     (insert -1)))

(defmodel first-sexp ()
  "Move back by sexps until you can go back no more."
  (interactive))

(defmodal first-sexp (fundamental-mode) ()
  "Move back by sexps until you can go back no more."
  (interactive)
  (let ((backto (safe-scan-sexps (point) -1)))
    (while backto
      (goto-char backto)
      (setq backto (safe-scan-sexps backto -1)))))

(defmodel last-sexp ()
  "Move forward by sexps until you can go no more."
  (interactive))

(defmodal last-sexp (fundamental-mode) ()
  "Move forward by sexps until you can go no more."
  (interactive)
  (let ((onto (safe-scan-sexps (point) 1))
	(prev (point))
	(prevprev (point)))
    (while onto
      (goto-char onto)
      (setq prevprev prev
	    prev onto
	    onto (safe-scan-sexps onto 1)))
    (parse-partial-sexp prevprev prev
			0 t)))

(defmodel next-sexp  (n)
  "Move forward N sexps.
  Like forward-sexp but moves to the start of the next sexp rather than the
  end of the current one, and so skips leading whitespace etc.
  See versor-allow-move-to-end-of-last for some finer control."
  (interactive "p"))
    
(defmodal next-sexp (fundamental-mode) (n)
  "Move forward N sexps.
  Like forward-sexp but moves to the start of the next sexp rather than the
  end of the current one, and so skips leading whitespace etc.
  See versor-allow-move-to-end-of-last for some finer control."
  (interactive "p")
  (let* ((where (safe-scan-sexps (point) n))
	 (one-more (safe-scan-sexps where 1)))
    ;; (message "where=%S one-more=%S" where one-more)
    (if where
	(progn
	  (goto-char where)
	  (let ((limit (save-excursion (last-sexp) (point))))
	    (when (> n 0)
	      (parse-partial-sexp where limit
				  0	; targetdepth
				  t	; stopbefore
				  )))
	  (when (and (not one-more) versor-allow-move-to-end-of-last)
	    ;; (message "not one-more; where=%d, point=%d" where (point)) ;
	    ;; This is the special case where we move to the end of
	    ;; the last regexp in a list. (The normal case is that
	    ;; we move to the start of a regexp, and let the surrounding
	    ;; macro versor-as-motion-command in versor-next call
	    ;; versor-set-current-item for us.
  (versor-set-current-item (safe-scan-sexps (point) -1)
			   ;; where
			   (if (eq versor-allow-move-to-end-of-last t)
			       where
			     (progn
			       (goto-char where)
			       ;; (message "Looking forward from %d" where)
			       ;; (skip-to-actual-code)
			       (skip-syntax-forward "^)")
			       (point)
			       ))
			   )))
      (if versor-move-out-when-at-end
	  (progn
	    (safe-up-list)
	    (skip-to-actual-code))
	(message "No more sexps")))))

(defmodal next-sexp (html-mode html-helper-mode) (n)
  "next-sexp for html.
Treats paired tags as brackets, and tries to do sensible
things with natural language punctuation."
  (cond
   ((save-excursion
      (skip-to-actual-code)
      (looking-at "<[^/]"))
    (skip-to-actual-code)
    (let ((start (point)))
      (nested-blocks-forward)
      (versor-set-current-item start (point))))
   ((save-excursion
      (let ((started (point)))
	(and (re-search-backward sentence-end (point-min) t)
	     (skip-to-actual-code)
	     (= started (point)))))
    (skip-to-actual-code)
    (let ((start (point)))
      (forward-sentence)
      (versor-set-current-item start (point))))
   ((looking-at "[[({]")
    (skip-to-actual-code)
    (let ((start (point)))
      (forward-sexp n)
      (versor-set-current-item start (point))))
   (t (forward-word 1))))

(defmodel previous-sexp (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p"))

(defmodal previous-sexp (fundamental-mode) (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p")
  (let ((where (safe-scan-sexps (point) (- n))))
    (if where
	(goto-char where)
      (if versor-move-out-when-at-end
	  (safe-backward-up-list)
	(message "No more previous sexps")))))

(defmodal previous-sexp (html-mode html-helper-mode) (n)
  "Move backward N html blocks."
  (cond
   ((save-excursion
      (let ((start (point)))
	(and (re-search-backward (nested-blocks-end) (point-min) t)
	     (progn (goto-char (match-end 0))
		    (skip-to-actual-code))
	     (>= (point) start))))
    (re-search-backward (nested-blocks-end) (point-min) t)
    (let ((end (match-end 0)))
      (goto-char end)
      (nested-blocks-backward)
      (versor-set-current-item (point) end)))
   ;; todo: add handling of sentences
   ;; todo: add handling of real brackets
   (t (backward-word 1))))

(defmodel innermost-list ()
  "Move in by sexps until you can go in no more."
  (interactive))

(defmodal innermost-list (fundamental-mode) ()
  "Move in by sexps until you can go in no more."
  (interactive)
  (let ((p (point))
	(n nil))
    (while (setq n (safe-scan-lists p 1 -1))
      (setq p n))
    (goto-char p)))

(defun versor-next-word (n)
  "Move forward a word, or, with argument, that number of words.
Like forward-word but leaves point on the first character of the word,
and never on the space or punctuation before it."
  (interactive "p")
  (forward-word n)
  (skip-syntax-forward "^w"))

(defun versor-end-of-word (n)
  "Move to the end of the current word, or, with argument, that number of words."
  (interactive "p")
  (forward-word (1- n))
  (skip-syntax-forward "w"))

(defun versor-delete-word (n)
  "Delete N words."
  (interactive "p")
  (versor-as-motion-command item
   (let* ((spaced (and (= (char-before (car item)) ? )
		       (= (char-after (cdr item)) ? ))))
     (delete-region (car item) (cdr item))
     (if spaced (just-one-space)))))

(defun forward-phrase (n)
  "Move forward a phrase, or, with argument, that number of phrases.
Stay within the current sentence."
  (interactive "p")
  (let ((sentence (save-excursion
		    (forward-sentence 1)
		    (point))))
    (while (> n 0)
      (unless (re-search-forward phrase-end sentence 'stay)
	(setq n 0))
      (decf n))))

(defun end-of-phrase (&rest ignore)
  "Move to the end of the current phrase."
  (interactive)
  (let ((sentence (save-excursion
		    (forward-sentence 1)
		    (point))))
    (if (re-search-forward phrase-end sentence t)
      (goto-char (match-beginning 0))
      (goto-char (1- sentence)))))

(defun backward-phrase (n)
  "Move backward a phrase.
Stay within the current sentence."
  (interactive "p")
  (let* ((old-point (point))
	 (sentence (save-excursion
		     (backward-sentence 1)
		     (point)))
	 (using-sentence-start nil)
	 (in-end-area (save-excursion
			(re-search-backward phrase-end sentence t)
			(if (>= (match-end 0) old-point)
			    (match-beginning 0)
			  nil))))
    (when in-end-area
      (goto-char in-end-area))
    (while (> n 0)
      (unless (re-search-backward phrase-end sentence 'stay)
	(setq n 0
	      using-sentence-start t))
      (decf n))
    (unless using-sentence-start
      (goto-char (match-end 0)))))

;; left over from trying a buffer selection dimension
;; (defun next-buffer ()
;;   "Select the next buffer in this window."
;;   (interactive)
;;   (let ((this-buffer (current-buffer)))
;;     (switch-to-buffer (other-buffer this-buffer))
;;     (bury-buffer this-buffer)))
;; 
;; (defun previous-buffer ()
;;   "Select the previous buffer in this window."
;;   (interactive)
;;   (switch-to-buffer (car (last (buffer-list)))))
;; 
;; (defun last-buffer ()
;;   "Select the last buffer in this window."
;;   (interactive)
;; )
;; 
;; (defun first-buffer ()
;;   "Select the first buffer in this window."
;;   (interactive)
;; )

(defun tempo-first-mark ()
  "Go to the first tempo marker."
  (interactive)
  (let ((first-mark (first tempo-marks)))
    (when first-mark (goto-char first-mark))))

(defun tempo-last-mark ()
  "Go to the last tempo marker."
  (interactive)
  (let ((last-mark (last tempo-marks)))
    (when last-mark (goto-char last-mark))))

(defun versor-set-mode-properties (mode properties)
  "Set the mode-specific versor properties for MODE to be PROPERTIES.
PROPERTIES is given as an alist."
  (mapcar (lambda (pair)
	    (put mode (car pair) (cdr pair)))
	  properties))

(versor-set-mode-properties
 'html-mode
 '((table-starter . "<table[^>]*>")
   (table-ender .  "</table[^>]*>")
   (row-starter . "<tr[^>]*>")
   (row-ender . "</tr[^>]*>")
   (cell-starter . "<t[dh][^>]*>")
   (cell-ender . "</t[dh][^>]*>")
   ))

(versor-set-mode-properties
 'html-helper-mode
 '((table-starter . "<table[^>]*>")
   (table-ender .  "</table[^>]*>")
   (row-starter . "<tr[^>]*>")
   (row-ender . "</tr[^>]*>")
   (cell-starter . "<t[dh][^>]*>")
   (cell-ender . "</t[dh][^>]*>")
   ))

(versor-set-mode-properties
 'texinfo-mode
 '((table-starter . "@multitable")
   (table-ender . "@end multitable")
   (row-starter . "@item")
   (cell-starter . "@tab")))

(versor-set-mode-properties
 'csv-mode
 '((table-starter . (lambda (a b)
		      (goto-char (point-min))
		      (set-match-data (list (point) (point)))))
   (table-ender . (lambda (a b)
		    (goto-char (point-max))
		    (set-match-data (list (point) (point)))))
   (row-starter . (lambda (a b)
		    (beginning-of-line 1)
		    (set-match-data (list (point) (point)))))
   (row-ender . (lambda (a b)
		  (end-of-line 1)
		  (set-match-data (list (point) (point)))))
   (next-row . (lambda (n)
		 (beginning-of-line (1+ n))))
   (previous-row . (lambda (n)
		     (beginning-of-line (- 1 n))))
   (cell-starter . (lambda (a b)
		     (csv-forward-field 1)
		     (set-match-data (list (point) (point)))))
   (cell-ender . (lambda (a b)
		   (csv-backward-field 1)
		   (set-match-data (list (point) (point)))))
   (next-cell . (lambda (n)
		  (csv-forward-field n)))
   (previous-cell . (lambda (n)
		      (csv-backward-field n)))))

(defun versor-table-starter ()
  "Return the table starter regexp for the current major mode."
  (or (get major-mode 'table-starter)
      (error "No table starter for %S") major-mode))

(defun versor-table-ender ()
  "Return the table ender regexp for the current major mode."
  (or (get major-mode 'table-ender)
      (error "No table ender for %S") major-mode))

(defun versor-row-starter ()
  "Return the row starter regexp for the current major mode."
  (or (get major-mode 'row-starter)
      (error "No row starter for %S") major-mode))

(defun versor-row-ender ()
  "Return the row ender regexp for the current major mode."
   (get major-mode 'row-ender))

(defun versor-row-next ()
  "Return the next row function for the current major mode."
   (get major-mode 'next-row))

(defun versor-row-previous ()
  "Return the previous row function for the current major mode."
   (get major-mode 'previous-row))

(defun versor-cell-starter ()
  "Return the cell starter regexp for the current major mode."
  (or (get major-mode 'cell-starter)
      (error "No cell starter for %S") major-mode))

(defun versor-cell-ender ()
  "Return the cell ender regexp for the current major mode."
  (get major-mode 'cell-ender))

(defun versor-cell-next ()
  "Return the next cell function for the current major mode."
   (get major-mode 'next-cell))

(defun versor-cell-previous ()
  "Return the previous cell function for the current major mode."
   (get major-mode 'previous-cell))


(defun re-search-forward-callable (regexp &optional bound noerror)
  "Like re-search-forward, but REGEXP can be a function.
If so, it is called on the other two arguments."
  (if (functionp regexp)
      (funcall regexp bound noerror)
    (re-search-forward regexp bound noerror)))

(defun re-search-backward-callable (regexp &optional bound noerror)
  "Like re-search-backward, but REGEXP can be a function.
If so, it is called on the other two arguments."
  (if (functionp regexp)
      (funcall regexp bound noerror)
    (re-search-backward regexp bound noerror)))

(defun versor-first-cell ()
  "Move to the first cell of the current row."
  (interactive)
  (if (and (re-search-backward-callable (versor-row-starter) (point-min) t)
	   (re-search-forward-callable (versor-cell-starter) (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor-previous-cell (n)
  "Move to the previous cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (let ((direct-action (versor-cell-previous)))
    (if direct-action
	(funcall direct-action n)
      (let* ((cell-starter (versor-cell-starter)))
	(while (> n -1)
	  (backward-char 1)
	  (re-search-backward-callable cell-starter (point-min) t)
	  (setq n (1- n)))
	(let ((start-starter (point))
	      (start-ender (match-end 0))
	      (ender (versor-cell-ender)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (forward-char 1)
	      (re-search-forward-callable (versor-cell-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-next-cell (n)
  "Move to the next cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (let ((direct-action (versor-cell-next)))
    (if direct-action
	(funcall direct-action n)
      (forward-char 1) ; so we can search for next starter even if already on one
      (let* ((starter (versor-cell-starter))
	     (ender (versor-cell-ender)))
	(while (> n 0)
	  (message "Searching from %d for starter %S" (point) starter)
	  (if (not (re-search-forward-callable starter (point-max) t))
	      (error "No more next cells")
	    (decf n)))
	(let ((starter-start (match-beginning 0))
	      (starter-end (point)))
	  (message "starter runs %d..%d, ender is %S" starter-start starter-end ender)
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (message "No ender, improvising by going just before next %S" starter)
	      (forward-char 1)
	      (re-search-forward-callable (versor-cell-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (message "got %d as end position" (point))
	    (versor-set-current-item starter-start (point)))
	  (goto-char starter-end)
	  (skip-to-actual-code)
	  (message "went to starter-end %d and skipped to %d" starter-end (point)))))))

(defun versor-last-cell ()
  "Move to the last cell of the current row."
  (interactive)
  ;; todo: finish this properly
  (if (and (re-search-forward-callable (versor-row-ender) (point-max) t)
	   (re-search-backward-callable (versor-cell-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor-first-row ()
  "Move to the first row of the current table."
  (interactive)
  ;; todo: finish this properly
  (if (and (search-backward (versor-table-starter) (point-min) t)
	   (re-search-forward-callable (versor-row-starter) (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor-previous-row (n)
  "Move to the previous row."
  (interactive "p")
  (let ((direct-action (versor-row-previous)))
    (if direct-action
	(funcall direct-action n)
      (let* ((limit (save-excursion
		      (re-search-backward-callable (versor-table-starter) (point-min) t)
		      (match-end 0)))
	     (row-starter (versor-row-starter))
	     (ender (versor-row-ender)))
	(while (> n -1)
	  (backward-char 1)
	  (re-search-backward-callable row-starter limit t)
	  (setq n (1- n)))
	(let ((start-starter (point))
	      (start-ender (match-end 0)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender (point-max) t)
	      (forward-char 1)
	      (re-search-forward-callable (versor-row-starter) (point-max) t)
	      (goto-char (1- (match-beginning 0))))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-next-row (n)
  "Move to the next row."
  (interactive "p")
  (let ((direct-action (versor-row-next)))
    (if direct-action
	(funcall direct-action n)
      (forward-char 1)
      (let* ((ender (versor-row-ender))
	     (limit (save-excursion
		      (re-search-forward-callable (versor-table-ender) (point-max) t)
		      (match-beginning 0)))
	     (row-starter (versor-row-starter)))
	(while (> n 0)
	  (if (re-search-forward-callable row-starter limit t)
	      (decf n)		
	    (setq n 0)))
	(let ((start-ender (point))
	      (start-starter (match-beginning 0)))
	  (save-excursion
	    (if ender
		(re-search-forward-callable ender limit t)
	      (forward-char 1)
	      (if (re-search-forward-callable (versor-row-starter) limit t)
		  (goto-char (1- (match-beginning 0)))
		(goto-char limit)))
	    (versor-set-current-item start-starter (point)))
	  (goto-char start-ender)
	  (skip-to-actual-code))))))

(defun versor-last-row ()
  "Move to the last row of the current table."
  (interactive)
  ;; todo: finish this properly
  (if (and (search-forward (versor-table-ender) (point-max) t)
	   (re-search-backward-callable (versor-row-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last row")))

(defun versor-first-defun ()
  "Move to the start of the first function definition in the buffer."
  (interactive)
  (goto-char (point-min))
  (beginning-of-defun -1))

(defun versor-previous-defun (n)
  "Move to the start of the previous function definition."
  (interactive "p")
  (beginning-of-defun n))

(defun versor-next-defun (n)
  "Move to the start of the next function definition."
  (interactive "p")
  (beginning-of-defun (- n)))

(defun versor-last-defun ()
  "Move to the start of the last function definition in the buffer."
  (interactive)
  (goto-char (point-max))
  (beginning-of-defun 1))

;;;; end of versor-base-moves.el
