;;; versor-base-moves.el -- versatile cursor
;;; Time-stamp: <2006-02-17 19:19:23 jcgs>
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
(provide 'versor-base-moves)

;; some internal functions for operations that aren't typically there already,
;; or that normal emacs does slightly differently from what we want

(defun versor:forward-paragraph (&optional n)
  "Like forward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (forward-paragraph n)
  (message "forward-paragraph %d; then forward over blank lines" n)
  (if (> n 0)
      (while (looking-at "^$")
	(message "skipping blank line at start of paragraph")
	(forward-line 1))))

(defun versor:backward-paragraph (&optional n)
  "Like backward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (backward-paragraph (1+ n))
  (message "backward-paragraph %d; then forward over blank lines" n)
  (if (> n 0)
      (while (looking-at "^$")
	(message "skipping blank line at start of paragraph")
	(forward-line 1))))

(defun versor:end-of-paragraph (&optional arg)
  "Move to the end of the paragraph."
  (end-of-paragraph-text))

;; todo: add tex, latex, bibtex ideas of paragraphs, perhaps going via generic-text, or building on the built-in ones in-place?

(defun versor:start-of-line (n)
  "Move to first non-space on line, or to start of Nth line from here."
  (interactive "p")
  (if (or (= n 1)
	  (not (eq last-command 'versor:start-of-line)))
      (back-to-indentation)
    (beginning-of-line n)))

(defmacro versor:following-margins (&rest body)
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

(defun versor:previous-line (n)
  "Move to the previous line, following margins if on one."
  (interactive "p")
  (versor:following-margins (previous-line n)))

(defun versor:next-line (n)
  "Move to the next line, following margins if on one."
  (interactive "p")
  (versor:following-margins (next-line n)))

(defun safe-backward-up-list (&rest args)
  "Like backward-up-list, but returns nil on error."
  (condition-case error-var
      (apply 'backward-up-list args)
    (error nil)))

(defun safe-down-list (&rest args)
  "Like down-list, but returns nil on error."
  (condition-case error-var
      (progn
	(apply 'down-list args)
	t)
    (error nil)))

(defvar versor:reformat-automatically t
  "If non-nil, some versor movements call reformatting commands.")

(defun versor:backward-up-list (&rest args)
  (interactive "p")
  (apply 'safe-backward-up-list args)
  (when versor:reformat-automatically
    (condition-case evar
	(indent-sexp)
      (error nil)))
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (let ((start (point))
	(end (save-excursion (forward-sexp 1) (point))))
    ;; TODO: should probably be versor:set-current-item
    (make-versor:overlay start (1+ start))
    ;; (message "extra overlay at %d..%d" (1- (point)) (point))
    ;; TODO: should probably be versor:add-to-current-item when I've written one
    (versor:extra-overlay (1- end) end)
    (cons start end)))

(defun versor:down-list (&rest args)
  (interactive "p")
  (unless (apply 'safe-down-list args)
    ;; if we were at the end of the last expression, try going to the start of it
    (previous-sexp 1)
    (safe-down-list args))
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (make-versor:overlay (point) (1+ (point))) ; TODO: should probably be versor:set-current-item
  (when (looking-at "\\s(")
    (save-excursion
      (forward-sexp 1)
      ;; (message "extra overlay at %d..%d" (1- (point)) (point))
      (versor:extra-overlay (1- (point)) (point))))) ; TODO: should probably be versor:add-to-current-item when I've written one

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

(defun first-sexp ()
  "Move back by sexps until you can go back no more."
  (interactive)
  (let ((backto (safe-scan-sexps (point) -1)))
    (while backto
      (goto-char backto)
      (setq backto (safe-scan-sexps backto -1)))))

(defun last-sexp ()
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

(defvar versor:allow-move-to-end-of-last 'dwim
  "*Whether to allow moving to the end of the last sexp in a list.
Otherwise, versor:next stops at the start of it, and refuses to do
another forward move.
Setting this non-nil does what you probably want in practice, although
setting it nil is probably cleaner in some abstract sort of way.
Setting it non-nil and not t will make the last move within a list
go to just before the closing syntax of the list, which is where you
typically want to be to type the next sexp in.")

(defvar versor:move-out-when-at-end t
  "If non-nil, trying to move further on when already at the end of
the last thing in a container (see versor:allow-move-to-end-of-last)
will move to just after the end of the container. Can be convenient in
practice, although it breaks the symmetry of the next<-->previous
operations.")

(defun next-sexp (n)
  "Move forward N sexps.
Like forward-sexp but moves to the start of the next sexp rather than the
end of the current one, and so skips leading whitespace etc.
See versor:allow-move-to-end-of-last for some finer control."
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
	  (when (and (not one-more) versor:allow-move-to-end-of-last)
	    (message "not one-more; where=%d, point=%d" where (point))
	    ;; This is the special case where we move to the end of
	    ;; the last regexp in a list. (The normal case is that
	    ;; we move to the start of a regexp, and let the surrounding
	    ;; macro versor:as-motion-command in versor:next call
	    ;; versor:set-current-item for us.
	    (versor:set-current-item (safe-scan-sexps (point) -1)
				     ;; where
				     (if (eq versor:allow-move-to-end-of-last t)
					 where
				       (progn
					 (goto-char where)
					 (message "Looking forward from %d" where)
					 ;; (skip-to-actual-code)
					 (skip-syntax-forward "^)")
					 (point)
					 ))
				     )))
      (if versor:move-out-when-at-end
	  (progn
	    (up-list)
	    (skip-to-actual-code))
	(message "No more sexps")))))

(defun previous-sexp (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p")
  (let ((where (safe-scan-sexps (point) (- n))))
    (if where
	(goto-char where)
      (if versor:move-out-when-at-end
	  (safe-backward-up-list)
	(message "No more previous sexps")))))

(defun innermost-list ()
  "Move in by sexps until you can go in no more."
  (interactive)
  (let ((p (point))
	(n nil))
    (while (setq n (safe-scan-lists p 1 -1))
      (setq p n))
    (goto-char p)))

(defun versor:next-word (n)
  "Move forward a word, or, with argument, that number of words.
Like forward-word but leaves point on the first character of the word,
and never on the space or punctuation before it."
  (interactive "p")
  (forward-word n)
  (skip-syntax-forward "^w"))

(defun versor:end-of-word (n)
  "Move to the end of the current word, or, with argument, that number of words."
  (interactive "p")
  (forward-word (1- n))
  (skip-syntax-forward "w"))

(defun versor:delete-word (n)
  "Delete N words."
  (interactive "p")
  (versor:as-motion-command
   (let* ((item (versor:get-current-item))
	  (spaced (and (= (char-before (car item)) ? )
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

(defun backward-phrase (n)
  "Move backward a phrase.
Stay within the current sentence."
  (interactive "p")
  (let ((sentence (save-excursion
		    (backward-sentence 1)
		    (point))))
    (while (> n 0)
      (unless (re-search-backward phrase-end sentence 'stay)
	(setq n 0))
      (decf n))))

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

(defvar versor:table-starters
  '((html-helper-mode . "<table[^>]*>")
    (html-mode . "<table[^>]*>")
    (texinfo-mode . "@multitable"))
  "Alist of table starter regexps for each major mode.")

(defvar versor:table-enders
  '((html-helper-mode . "</table[^>]*>")
    (html-mode . "</table[^>]*>")
    (texinfo-mode . "@end multitable"))
  "Alist of table ender regexps for each major mode.")

(defvar versor:row-starters
  '((html-helper-mode . "<tr[^>]*>")
    (html-mode . "<tr[^>]*>")
    (texinfo-mode . "@item"))
  "Alist of row starter regexps for each major mode.")

(defvar versor:row-enders
  '((html-helper-mode . "</tr[^>]*>")
    (html-mode . "</tr[^>]*>"))
  "Alist of row ender regexps for each major mode.")

(defvar versor:cell-starters
  '((html-helper-mode . "<t[dh][^>]*>")
    (html-mode . "<t[dh][^>]*>")
    (texinfo-mode . "@tab"))
  "Alist of cell starter regexps for each major mode.")

(defvar versor:cell-enders
  '((html-helper-mode . "</t[dh][^>]*>")
    (html-mode . "</t[dh][^>]*>"))
  "Alist of cell ender regexps for each major mode.")

(defun versor:table-starter ()
  "Return the table starter regexp for the current major mode."
  (or (cdr (assoc major-mode versor:table-starters))
      (error "No table starter for %S") major-mode))

(defun versor:table-ender ()
  "Return the table ender regexp for the current major mode."
  (or (cdr (assoc major-mode versor:table-enders))
      (error "No table ender for %S") major-mode))

(defun versor:row-starter ()
  "Return the row starter regexp for the current major mode."
  (or (cdr (assoc major-mode versor:row-starters))
      (error "No row starter for %S") major-mode))

(defun versor:row-ender ()
  "Return the row ender regexp for the current major mode."
   (cdr (assoc major-mode versor:row-enders)))

(defun versor:cell-starter ()
  "Return the cell starter regexp for the current major mode."
  (or (cdr (assoc major-mode versor:cell-starters))
      (error "No cell starter for %S") major-mode))

(defun versor:cell-ender ()
  "Return the cell ender regexp for the current major mode."
  (cdr (assoc major-mode versor:cell-enders)))

(defun versor:first-cell ()
  "Move to the first cell."
  (interactive)
  (if (and (search-backward (versor:row-starter) (point-min) t)
	   (re-search-forward (versor:cell-starter) (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor:previous-cell (n)
  "Move to the previous cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (while (> n 0)
    (backward-char 1)
    (re-search-backward (versor:cell-starter) (point-min) t)
    (setq n (1- n)))
  (save-excursion
    (let ((start (point))
	  (ender (versor:cell-ender)))
      (if ender
	  (re-search-forward ender (point-max) t)
	(forward-char 1)
	(re-search-forward (versor:cell-starter) (point-max) t)
	(goto-char (1- (match-beginning 0))))
      (versor:set-current-item start (point)))))

(defun versor:next-cell (n)
  "Move to the next cell."
  ;; todo: limit to within this row? or at least not include the row markup line
  (interactive "p")
  (forward-char 1)
  (while (> n 0)
    (if (not (re-search-forward (versor:cell-starter) (point-max) t))
	(error "No more next cells")
      (decf n)))
  (goto-char (match-beginning 0))
  (let ((start (point)))
    (save-excursion
      (let ((ender (versor:cell-ender)))
	(if ender
	    (re-search-forward ender (point-max) t)
	  (forward-char 1)
	  (re-search-forward (versor:cell-starter) (point-max) t)
	  (goto-char (1- (match-beginning 0)))))
      (versor:set-current-item start (point)))))

(defun versor:last-cell ()
  "Move to the last cell."
  (interactive)
  (if (and (re-search-forward (versor:row-ender) (point-max) t)
	   (re-search-backward (versor:cell-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor:first-row ()
  "Move to the first row."
  (interactive)
  (if (and (search-backward (versor:table-starter) (point-min) t)
	   (re-search-forward (versor:row-starter) (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor:previous-row (n)
  "Move to the previous row."
  (interactive "p")
  (let ((limit (save-excursion
		 (re-search-backward (versor:table-starter) (point-min) t)
		 (match-end 0))))
    (while (> n 0)
      (backward-char 1)
      (re-search-backward (versor:row-starter) limit t)
      (setq n (1- n)))
    (save-excursion
      (let ((start (point))
	    (ender (versor:row-ender)))
	(if ender
	    (re-search-forward ender (point-max) t)
	  (forward-char 1)
	  (re-search-forward (versor:row-starter) (point-max) t)
	  (goto-char (1- (match-beginning 0))))
	(versor:set-current-item start (point))))))

(defun versor:next-row (n)
  "Move to the next row."
  ;; todo: handling of last row isn't right yet
  (interactive "p")
  (forward-char 1)
  (let ((limit (save-excursion
		 (re-search-forward (versor:table-ender) (point-max) t)
		 (match-beginning 0))))
    (while (> n 0)
      (if (not (re-search-forward (versor:row-starter) limit t))
	  (error "No more next rows")
	(decf n)))
    (goto-char (match-beginning 0))
    (let ((start (point)))
      (save-excursion
	(let ((ender (versor:row-ender)))
	  (if ender
	      (re-search-forward ender limit t)
	    (forward-char 1)
	    (re-search-forward (versor:row-starter) limit t)
	    (goto-char (1- (match-beginning 0)))))
	(versor:set-current-item start (point))))))

(defun versor:last-row ()
  "Move to the last row."
  (interactive)
  (if (and (search-forward (versor:table-ender) (point-max) t)
	   (re-search-backward (versor:row-starter) (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last row")))

(defun versor:first-defun ()
  "Move to the start of the first function definition in the buffer."
  (interactive)
  (goto-char (point-min))
  (beginning-of-defun -1))

(defun versor:previous-defun (n)
  "Move to the start of the previous function definition."
  (interactive "p")
  (beginning-of-defun n))

(defun versor:next-defun (n)
  "Move to the start of the next function definition."
  (interactive "p")
  (beginning-of-defun (- n)))

(defun versor:last-defun ()
  "Move to the start of the last function definition in the buffer."
  (interactive)
  (goto-char (point-max))
  (beginning-of-defun 1))

;;;; end of versor-base-moves.el
