;;; versor-base-moves.el -- versatile cursor
;;; Time-stamp: <2004-07-16 18:40:56 john>
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

(require 'cl)
(provide 'versor-base-moves)

;; some internal functions for operations that aren't typically there already,
;; or that normal emacs does slightly differently from what we want

(defun versor:forward-paragraph (&optional n)
  "Like forward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (forward-paragraph n)
  (message "forward-paragraph %d" n)
  (if (> n 0)
      (while (looking-at "^$")
	(forward-line 1))))

(defun versor:backward-paragraph (&optional n)
  "Like backward-paragraph, but don't end up on a blank line."
  (interactive "p")
  (backward-paragraph (1+ n))
  (message "backward-paragraph %d" n)
  (if (> n 0)
      (while (looking-at "^$")
	(forward-line 1))))

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

(defun versor:backward-up-list (&rest args)
  (interactive "p")
  (apply 'backward-up-list args)
  ;; (message "main overlay at %d..%d" (point) (1+ (point)))
  (make-versor:overlay (point) (1+ (point))) ; TODO: should probably be versor:set-current-item
  (save-excursion
    (forward-sexp 1)
    ;; (message "extra overlay at %d..%d" (1- (point)) (point))
    (versor:extra-overlay (1- (point)) (point)))) ; TODO: should probably be versor:add-to-current-item when I've written one

(defun versor:down-list (&rest args)
  (interactive "p")
  (apply 'down-list args)
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

(defun next-sexp (n)
  "Move forward N sexps.
Like forward-sexp but moves to the start of the next sexp rather than the
end of the current one, and so skips leading whitespace etc."
  (interactive "p")
  (let* ((where (safe-scan-sexps (point) n))
	 (one-more (safe-scan-sexps where 1)))
    ;; (message "where=%S one-more=%S" where one-more)
    (if (and where
	     ;; one-more
	     )
	(progn
	  (goto-char where)
	  (let ((limit (save-excursion (last-sexp) (point))))
	    (when (> n 0)
	      (parse-partial-sexp where limit
				  0 t)))
	  (unless one-more
	    (versor:set-current-item where
				     (safe-scan-sexps (point) -1))))
      (message "No more sexps"))))

(defun previous-sexp (n)
  "Move backward N sexps.
Like backward-sexp but stops without error on reaching the start."
  (interactive "p")
  (let ((where (safe-scan-sexps (point) (- n))))
    (if where
      (goto-char where)
      (message "No more previous sexps"))))

(defun innermost-list ()
  "Move in by sexps until you can go in no more."
  (interactive)
  (let ((p (point))
	(n nil))
    (while (setq n (safe-scan-lists p 1 -1))
      (setq p n))
    (goto-char p)))

(defun next-word (n)
  "Move forward a word, or, with argument, that number of words.
Like forward-word but leaves point on the first character of the word,
and never on the space or punctuation before it."
  (interactive "p")
  (forward-word n)
  (skip-syntax-forward "^w"))

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

(defun versor:first-cell ()
  "Move to the first cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-backward "<tr" (point-min) t)
	   (re-search-forward "<t[dh][^>]+" (point-max) t))
      t
    (error "Could not locate first cell.")))

(defun versor:previous-cell (n)
  "Move to the previous cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (if (re-search-backward "</t[dh][^>]+" (point-min) t)
      (progn
	(while (> n 0)
	  (if (not (re-search-backward "<t[dh][^>]+" (point-min) t))
	      (error "No more previous cells")
	    (decf n)))
	(goto-char (match-end 0)))
    (error "Could not end of previous locate cell")))

(defun versor:next-cell (n)
  "Move to the next cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<t[dh][^>]+" (point-max) t))
	(error "No more next cells")
      (decf n))))

(defun versor:last-cell ()
  "Move to the last cell."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-forward "</tr" (point-max) t)
	   (re-search-backward "<t[dh][^>]+" (point-min) t))
      (goto-char (match-end 0))
    (error "Could not locate last cell")))

(defun versor:first-row ()
  "Move to the first row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-backward "<table" (point-min) t)
	   (re-search-forward "<tr[^>]+" (point-max) t))
      t
    (error "Could not locate first row")))

(defun versor:previous-row (n)
  "Move to the previous row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (if (re-search-backward "</tr[^>]+" (point-min) t)
      (progn
	(while (> n 0)
	  (if (not (re-search-backward "<tr[^>]+" (point-min) t))
	      (error "No more previous rows")
	    (decf n)))
	(goto-char (match-end 0)))
    (error "Could not end of previous locate row")))

(defun versor:next-row (n)
  "Move to the next row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive "p")
  (while (> n 0)
    (if (not (re-search-forward "<tr[^>]+" (point-max) t))
	(error "No more next rows")
      (decf n))))

(defun versor:last-row ()
  "Move to the last row."
  ;;;;;;;;;;;;;;;; make this understand TeX and LaTeX too!!!!!!!!!!!!!!!!"
  (interactive)
  (if (and (search-forward "</table" (point-max) t)
	   (re-search-backward "<tr[^>]+" (point-min) t))
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
