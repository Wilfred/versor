;;;; languide-c-like.el -- C, java, perl definitions for language-guided editing
;;; Time-stamp: <2006-04-10 13:55:51 john>
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
(provide 'languide-c-like)

(defun languide-c-back-to-possible-ender (bod)
  "Move point back to be at something that might end a C statement,
and is outside a comment or string, relative to BOD.
BOD is Beginning Of Defun, which is taken to be not in a comment or string."
  (let ((in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (cond
       ((and nil (looking-at "[a-z_]"))	; inside a symbol -- go to start of symbol -- not sure why I wanted to do this -- anded out for now
	(skip-syntax-backward "w_")
	)
       (t
	(re-search-backward "[{;}]"
			    bod		; (point-min)
			    t)	      ; leaves point at start of match
	(languide-debug-message 'languide-c-back-to-possible-ender "Found a \"%s\" at %d" (match-string 0) (point))
	;; having found a character that can end a C statement,
	;; we now parse from the start of the defun up to the
	;; position of the character, to see whether the character
	;; is in code or in string-or-comment
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (languide-debug-message 'languide-c-back-to-possible-ender "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (if (nth 3 result) t nil)	; only want a number in in-comment-or-string if it tells us a character position; this one gives us a character code
				      (nth 4 result)
				      (languide-c-inside-for-control)
				      ))
	  (languide-debug-message 'languide-c-back-to-possible-ender "in-comment-or-string=%S" in-comment-or-string)
	  (if in-comment-or-string
	      (if (numberp in-comment-or-string)
		  (goto-char in-comment-or-string)
		(backward-char 1)))))))))

(defun languide-c-inside-for-control ()
  "Return whether we seem to be in the head of a for loop.
Useful when we've got to a semicolon and need to know whether it terminates a statement.
If not at a semicolon (e.g. deeper inside an expression inside the for loop head) this
is liable to return the wrong result."
  (save-excursion
    (condition-case error-var
	(progn
	  (backward-up-list 1)
	  (backward-sexp 1)
	  (looking-at "for"))
      (error nil))))

(defun blank-between (a b)
  "Return whether there is only whitespace between A and B."
  (save-excursion
    (goto-char a)
    (= (- b a)
       (skip-chars-forward " \t\n\r" b))))

(defmodal move-into-previous-statement (c-mode perl-mode) ()
  "Move into the previous C or Perl statement.
Need only work if already at or just before the start of a statement."
  (interactive)
  (let ((done nil))
    (while (not done)
      (skip-chars-backward " \t\n")
      (cond
       ((save-excursion
	  (backward-char 2)
	  (looking-at "\\*/"))
	(search-backward "/*" (point-min) t))
       ((eq (char-after (1- (point))) ?})
	(backward-sexp 1)
	(setq done t))
       (t
	(backward-char 1)
	(setq done t))))))

(defmodal move-into-next-statement (c-mode perl-mode) ()
  "Move into the next C or Perl statement.
Need only work if already at or just beyond the end of a statement."
  (interactive)
  (skip-to-actual-code)
  (if (looking-at "{")
      (forward-sexp 1)
    (forward-char 1)))

(defun languide-previous-substatement ()
  "Go back a block, if at a block end, otherwise go back a statement."
;;;;;;;;;;;;;;;; check first what kind, do things conditionally!
  (goto-char (safe-scan-sexps (point) -1))
)

(defvar c-like-function-type-modifiers
  (concat "\\(" (mapconcat 'identity
			   '("static" "struct" "extern" "public" "private")
			   "\\)\\|\\(")
	  "\\)")
  "Things that can appear before the actual types of functions.")

(defun continue-back-past-curly-ket (starting-point)
  "Continue to back to the start of a C statement, having got back to a closing curly bracket."
  ;; taken out-of-line for readability of surrounding code
  (let* ((close (point))
	 (following-code (skip-to-actual-code)))
    (languide-debug-message 'continue-back-past-curly-ket "From starting-point at %d, found close at %d, following code at %d is \"%s\"" starting-point close following-code (buffer-substring following-code (+ following-code 20)))
    (cond
     ;; first two cases are those where a keyword can follow a closing brace
     ;; and still be within a statement
     ((looking-at "\\<while\\>")
      (languide-debug-message 'continue-back-past-curly-ket "Got \"while\" at %d, looking back to see which kind" (point))
      ;; might be
      ;;   do { ... } * while ( ... )
      ;; or
      ;;   { ... } * while ( ... ) { ... } // with the first { ... } being a free-standing compound statement
      ;; and so must go back to check for the "do"
      ;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
      (goto-char (safe-scan-sexps (point) -2))
      (languide-debug-message 'continue-back-past-curly-ket "Going back to check for \"do\"  gets us to %d:\"%s\"" (point) (buffer-substring (point) (+ (point) 20)))
      (if (looking-at "\\<do\\>")
	  (languide-debug-message 'continue-back-past-curly-ket "at \"do\", so remaining there")
	(languide-debug-message 'continue-back-past-curly-ket "was not \"do\", going back to code after closing brace")
	;; go back to the "while"
	(goto-char following-code)))
     ((looking-at "\\<else\\>")
      ;; where we started was:
      ;;   if ( ... ) { ... } * else { ... }
      ;; and so we must go back over the "then", the condition, and the keyword
      (languide-debug-message 'continue-back-past-curly-ket "Got \"else\" following closing brace, so going back")
      ;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
      (goto-char (safe-scan-sexps (point) -3))
      (languide-debug-message 'continue-back-past-curly-ket "That got us to \"%s\" which I hope is an \"if\"" (buffer-substring (point) (+ (point) 20))))

     ;; having dealt with both the "{ ... } <keyword>" cases,
     ;; now see whether we started inside a statement that follows a closing brace
     ((>= starting-point following-code)
      ;; this case looks like:
      ;;   { ... } ab*cd // where the { ... } is a free-standing compound statement, or anything else
      ;; so we want to go to:
      ;;   { ... } *abcd
      (languide-debug-message 'continue-back-past-curly-ket "We started inside the statement following a closing brace, so go to the start of that statement")
      (goto-char following-code))

     ;; next, try going back a statement and a keyword
     ;; and seeing if that is an else
     ((save-excursion ; don't disturb things for the next clause's test
	;; we are looking for the cases like:
	;;   if (...) { ... } else { ... } *
	;; (languide-previous-substatement) ; we know it's a compound statement, or we wouldn't be in this function
	(goto-char (safe-scan-sexps close -2))
	;; let's see if that's got us to:
	;;   if (...) { ... } *else { ... }
	(looking-at "\\<else\\>"))
      (languide-debug-message 'continue-back-past-curly-ket "Got else")
      (goto-char (safe-scan-sexps close -2)) ; re-do the movement that we did inside the save-excursion above
      (languide-previous-substatement)
      (goto-char (safe-scan-sexps (point) -2))
      )

     ((save-excursion ; don't disturb things for the next clause's test
	;; todo: add handling for java's "throws", etc
	;; we are looking for the cases like:
	;;   void function (...) { ... } *
	(goto-char (safe-scan-sexps close -2))
	(looking-at "("))
      (languide-debug-message 'continue-back-past-curly-ket "got start of arglist")
      ;; re-do the movement that we did inside the save-excursion
      ;; above, and then carry on back past the function name and type
      (goto-char (safe-scan-sexps close -4))
      (languide-debug-message 'continue-back-past-curly-ket "Hope to be at function type and name at %d: \"%s\"" (point) (buffer-substring-no-properties (point) (+ 24 (point))))
      (let ((type-possible-start-start (point)))
	(while (progn
		 (backward-word 1)
		 (looking-at c-like-function-type-modifiers))
	  (setq type-possible-start-start (point)))
	(goto-char type-possible-start-start)))
      

     ;; if it wasn't an else, try going back another sexp
     ((progn
	(goto-char (safe-scan-sexps close -3))
	(looking-at "\\<\\(if\\)\\|\\(while\\)\\|\\(for\\)\\|\\(until\\)\\>"))
      (languide-debug-message 'continue-back-past-curly-ket "Got if/while/for/until, staying there")
      )

     (t
      ;; Go back and see what was before the brace; could be
      ;;   if ( ... ) { ... }
      ;;   while ( ... ) { ... }
      ;;   until ( ... ) { ... }
      ;;   for ( ... ) { ... }
      ;; or it could just be a free-standing code block
      (languide-debug-message 'continue-back-past-curly-ket "Other case of closing brace, at \"%s\"" (buffer-substring (point) (+ (point) 20)))


      (languide-debug-message 'continue-back-past-curly-ket "Other case; going back to look before the brace... got \"%s\"" (buffer-substring (point) (+ (point) 20)))
      (languide-debug-message 'continue-back-past-curly-ket "Remaining there contentedly")
      (cond


       (t
	(languide-debug-message 'continue-back-past-curly-ket "Before the brace was not if/while/for/until/else; assuming plain block")
	(goto-char (safe-scan-sexps close -1))))))
    (languide-debug-message 'continue-back-past-curly-ket "finished classifying code at close")
    ))

(defvar debug-overlays nil)

(defun debug-overlay (from to text colour)
  (when nil t 
    (let ((o (make-overlay from to)))
      (overlay-put o 'face (cons 'background-color colour))
      (overlay-put o 'before-string (format "[%s:" text))
      (overlay-put o 'after-string (format ":%s]" text))
      (if nil
	  (push o debug-overlays)
	(read-char)
	(delete-overlay o)))))

(defmodal beginning-of-statement-internal (c-mode perl-mode) ()
  "Move to the beginning of a C or Perl statement."
  (interactive)
  (mapcar 'delete-overlay debug-overlays)
  (setq debug-overlays nil)
  (let ((starting-point (point))
	;; get beginning of defun, so we can use parse-partial-sexp to
	;; see whether we have landed in a string or comment
	(bod (save-excursion
	       (c-beginning-of-defun 1)
	       (point))))
    (debug-overlay bod starting-point "back to bod" "red")
    (languide-debug-message 'beginning-of-statement-internal "")
    (languide-debug-message 'beginning-of-statement-internal "Starting beginning-of-statement-internal-c-perl at %d, with beginning-of-defun at %d" starting-point bod)

    (languide-c-back-to-possible-ender bod)

    (let ((possible-ender (point)))
      (debug-overlay possible-ender (1+ possible-ender) "possible-ender" "orange")
      (languide-debug-message 'beginning-of-statement-internal "now at possible ender %d: \"%s\", which is not in a comment or string" (point) (buffer-substring (point) (min (point-max) (+ (point) 20))))

      ;; We've now found a statement delimiter, and checked that it is a
      ;; real one, and not part of a string or comment. Now we might make
      ;; some adjustments, then finally move over any whitespace or comments
      ;; leading in to the actual statement.

      (cond
       ((looking-at "{")
	(debug-overlay (point) (1+ (point)) "found open as possible ender" "cyan")
	(languide-debug-message 'beginning-of-statement-internal "found open as possible ender")
	;; move one character forward (into the braced area) so that
	;; when we skip whitespace and comments, we will be at the
	;; start of the first statement in the braces
	(forward-char 1))

       ((looking-at "}")
	(debug-overlay (point) (1+ (point)) "found close as possible ender" "cyan")
	(languide-debug-message 'beginning-of-statement-internal "found close as possible ender at %d" (point))
	(forward-char 1)
	;; this will take us back past the rest of the statement
	;; todo: which is wrong in some cases, so fix that! Only go
	;; back if what is ahead of us is something that can follow a
	;; closing brace within a statement, so for example we should
	;; go back if at "do { ... }  * while (...)", but not if at
	;; "while (...) do { ... } *"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	(continue-back-past-curly-ket
	 ;; starting-point	   ; is this the right place to start it from? no, I don't think so
	 (point)
	 )
	(languide-debug-message 'beginning-of-statement-internal "gone back from close as possible ender, to %d" (point))
	)

       ((looking-at ";")
	(goto-char (match-end 0))
	(let ((following-code (skip-to-actual-code)))
	  (debug-overlay (point) (1+ (point)) "found semicolon as possible ender" "cyan")
	  (languide-debug-message 'beginning-of-statement-internal "found semicolon as possible ender at %d, code following it is at %d" (point) following-code)
	  ;; We have looked back from point, and found that the
	  ;; nearest thing that could be the end of the previous
	  ;; statement is a semicolon. then we've gone forward to the
	  ;; first thing that is not a space or comment, and by
	  ;; comparing a that position with that of the semicolon, we
	  ;; can work out whether we have just gone straight to the
	  ;; start of the current statement, or whether it is some
	  ;; other case, such as the else part of an if-then-else
	  (if (<= following-code starting-point)
	      (progn
		;; this is the "abcd;    fg*hi" case
		(goto-char following-code)
		(debug-overlay (point) (1+ (point)) "code following semicolon is behind or at where we started, so here we are" "pale green")
		(languide-debug-message 'beginning-of-statement-internal "code following semicolon is behind or at where we started, so here we are")
		(if (looking-at "else")
		    (progn
		      ;; this is the "if (xyz) abcd;    el*se mnop;" case
		      (languide-debug-message 'beginning-of-statement-internal "Looking at else")
		      (goto-char (- possible-ender 1)) ; just into the "then" statement
		      (beginning-of-statement-internal)
		      ;;;;;;;;;;;;;;;; probably more to do here?
		      )))
	    (progn
	      ;; this is the "abcd;  *  fghi" case
	      (debug-overlay (point) (1+ (point)) "code following semicolon is after where we started, go back another statement" "pale green")
	      (languide-debug-message 'beginning-of-statement-internal "code following semicolon is after where we started, so we were in the space between two statements; therefore go back another statement")
	      (goto-char (- possible-ender 1))
	      (languide-debug-message 'beginning-of-statement-internal "beginning-of-statement-internal recursing")
	      (beginning-of-statement-internal)
	      (languide-debug-message 'beginning-of-statement-internal "beginning-of-statement-internal done recursing"))))
	(when (save-excursion ; this is old version, is it right????????????????
		(parse-partial-sexp (point) starting-point
				    nil	; targetdepth
				    t	; stopbefore
				    )
		(= (point) starting-point))
	  (debug-overlay (min starting-point (point)) (max starting-point (point))
			 "no code between it and where we were" "blue")
	  (languide-debug-message 'beginning-of-statement-internal
				  "no code between it and where we were (%d); am now at %d"
				  starting-point (point))
	  (backward-char 1)
	  ))))

    ;; now we're at a real statement delimiter
    (debug-overlay (point) (1+ (point)) "back to real delimiter" "yellow")
    (languide-debug-message 'beginning-of-statement-internal
			    "Now at real delimiter, skip to code; am at %d, starting-point is %d"
			    (point) starting-point)
    ;;;;;;;;;;;;;;;; why this "unless"? find out, and comment it!
    (if (blank-between (point) (1+ starting-point))
	(languide-debug-message 'beginning-of-statement-internal
				"in blank area at %d..%d" (point) (1+ starting-point))
      (languide-debug-message 'beginning-of-statement-internal
			      "final step of b-o-s-i: not in blank; at %d: \"%s\"" (point) (buffer-substring-no-properties (point) (+ 24 (point))))
      (skip-to-actual-code starting-point)
      (languide-debug-message 'beginning-of-statement-internal "skip-to-actual-code with limit of %d got to %d: \"%s\"" starting-point (point) (buffer-substring-no-properties (point) (+ 24 (point))))
      )
      (debug-overlay (point) starting-point "Finished" "purple"))
  (languide-debug-message 'beginning-of-statement-internal
			  "beginning-of-statement-internal finished at %d" (point)))

(defmodal end-of-statement-internal (c-mode perl-mode java-mode) ()
  "Move to the end of a C, Perl or Java statement."
  (let ((old (point))
	;; get beginning of defun, so we can see whether we have landed in a
	;; string or comment
	(bod (save-excursion
	       (c-beginning-of-defun 1)
	       (point))))
    (languide-debug-message 'end-of-statement-internal "Starting end-of-statement-internal at %d, with beginning-of-defun at %d" old bod)
    ;; (if (looking-at "{") (backward-char 1))
    (let ((in-comment-or-string t))
      ;; keep looking for the possible start of a statement, and checking that
      ;; it is not part of a comment or string
      (while in-comment-or-string
	(re-search-forward "[{;}]" (point-max) t) ; leaves point at end of match
	(languide-debug-message 'end-of-statement-internal "Found a \"%s\" at %d" (match-string 0) (point))
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (languide-debug-message 'end-of-statement-internal "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (languide-c-inside-for-control)
				      (if (nth 3 result) t nil)
				      (nth 4 result)
				      ))
	  (languide-debug-message 'end-of-statement-internal "in-comment-or-string=%S" in-comment-or-string)
	  (if in-comment-or-string
	      (forward-char 1)))))

    (languide-debug-message 'end-of-statement-internal "Now at %d=%c, which is not in a comment or string; the latest match, which is \"%s\", starts at %d"
			    (point) (char-after (point)) (match-string 0) (match-beginning 0))
    (cond
     ((= (char-after (1- (point))) ?{)
      (languide-debug-message 'end-of-statement-internal "Found block start")
      (backward-char 1)
      (forward-sexp 1)))
    (cond
     ((save-excursion
	(languide-debug-message 'end-of-statement-internal "skipping from %d to look for else" (point))
	(skip-to-actual-code)
	(languide-debug-message 'end-of-statement-internal "skipped to %d to look for else" (point))
	(looking-at "else"))
      (languide-debug-message 'end-of-statement-internal "Found ELSE")
      (forward-sexp 1)
      (end-of-statement-internal)
      )
     ((save-excursion
	(skip-to-actual-code)
	(looking-at "while"))
      ;; now look around to see whether this is part of "do {} while ()"
      ;; avert your eyes when reading this code, or re-write it for me!
      (when (save-excursion
	      (skip-to-actual-code)
	      (forward-sexp 2)
	      (if (looking-at "[ \t\r\n]")
		  (skip-to-actual-code))
	      (looking-at "[;}]"))
	(languide-debug-message 'end-of-statement-internal "That WHILE is part of a DO")
	(skip-to-actual-code)
	(forward-sexp 2)
	(if (looking-at "[ \t\r\n]")
	    (skip-to-actual-code))))))
  (languide-debug-message 'end-of-statement-internal
			  "end-of-statement-internal finished at %d" (point)))

(defvar c-binding-regexp-1
  "\\([a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-+\\([a-z][a-z0-9_]*\\)\\s-*\\(=[^;]*\\)?;"
  "A regexp for a common kind of C binding.")

(defvar c-binding-regexp-2
  (concat "\\(\\(?:struct\\|unsigned\\|const\\|volatile\\)\\s-+[a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-*"
	  "\\([a-z][a-z0-9_]*\\)\\s-*\\(=[^;]*\\)?;")
  "A regexp for a common kind of C binding.")

(defvar c-binding-regexp-3
  (concat "\\(\\(?:const\\|volatile\\)\\s-+\\(?:struct\\|unsigned\\)\\s-+[a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-*"
	  "\\([a-z][a-z0-9_]*\\)\\s-*\\(=[^;]*\\)?;")
  "A regexp for a common kind of C binding.")

(defvar c-arg-regexp-1
  "\\([a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-+\\([a-z][a-z0-9_]*\\)\\s-*,?"
  "A regexp for a common kind of C arg.")

(defvar c-arg-regexp-2
  (concat "\\(\\(?:struct\\|unsigned\\|const\\|volatile\\)\\s-+[a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\)\\s-*"
	  "\\([a-z][a-z0-9_]*\\)\\s-*,?")
  "A regexp for a common kind of C arg.")

(defvar c-arg-regexp-3
  (concat "\\(\\(?:const\\|volatile\\)\\s-+\\(?:struct\\|unsigned\\)\\s-+[a-z][a-z0-9_]*\\s-*\\*?\\[?\\]?\\) *"
	  "\\([a-z][a-z0-9_]*\\)\\s-*,?")
  "A regexp for a common kind of C arg.")

(defmodal identify-statement (c-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "\\(do\\)\\|\\(for\\)\\|\\(while\\)\\|\\(if\\)\\|\\(return\\)\\|\\(switch\\)\\|\\(continue\\)\\|\\(default\\)\\|\\(case\\)")
    (languide-debug-message 'identify-statement "identify-statement-c-mode found %s" (match-string 0))
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((string= keyword-string "if")
	(save-excursion
	  (if (and (safe-forward-sexp 2)
		   (statement)
		   (skip-to-actual-code)
		   (looking-at "else"))
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "return") 'return)
       ((string= keyword-string "while") 'while-do)
       ((string= keyword-string "switch") 'switch)
       ((string= keyword-string "continue") 'continue)
       ((string= keyword-string "case") 'case)
       ((string= keyword-string "default") 'default)
       ((string= keyword-string "do") 'do-while)
       (t nil))))
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *[-+*/]?= *")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at "\\(++\\)\\|\\(--\\)")))
    'assignment)
   ((save-excursion
      (and (safe-forward-sexp 1)
	   (looking-at " *(")))
    'function-call)
   ;; todo: recognize variable declarations with and without initial values
   ((save-excursion
      (and (safe-forward-sexp 2)
	   (looking-at " *[=;] *")))
    ;; (message "vardef1")
    'variable-declaration)
   ((save-excursion
      (looking-at "[^;()=]+;"))
    ;; (message "vardef2")
    'variable-declaration)
   ((looking-at c-binding-regexp-1)
    ;; (message "vardef3")
    'variable-declaration)
   ((eq (char-after (point)) ?{)
    'progn)
   ((and (zerop (current-column))
	 (looking-at "\\(static\\|extern\\)? *\\(struct +\\)?\\*?\\([a-z][a-z0-9_]*\\)"))
    'defun)
   (t default)))

(defmodal insert-compound-statement-open (c-mode java-mode perl-mode) ()
  "Insert a block start."
  (languide-insert "{\n"))

(defmodal compound-statement-open (c-mode java-mode perl-mode) ()
  "Return a block start."
   "{")

(defmodal insert-compound-statement-close (c-mode java-mode perl-mode) ()
  "Insert a block end."
  (languide-insert "\n}"))

(defmodal compound-statement-close (c-mode java-mode perl-mode) ()
  "Return a block end."
  "}")

(defmodal statement-container (c-mode perl-mode java-mode) ()
  "Select the container of the current statement."
  ;; needs to do the "not in string, not in comment" stuff, so we need 
  ;; the Beginning Of Defun to compare against
  (let* ((bod (save-excursion
		(c-beginning-of-defun 1)
		(point)))
	 (in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      ;; todo: this is wrong, it can find a preceding and closed container
      ;; (search-backward "{" (point-min) t) ; leaves point at end of match
      (if (backward-up-list)
	  (let ((result (save-excursion (parse-partial-sexp bod (point)
							    0 ; target-depth
							    nil	; stop-before
							    nil	; state
							    nil	; stop-comment
							    ))))
	    (languide-debug-message 'statement-container "parse-partial-sexp returned %S" result)
	    (setq in-comment-or-string (or
					;; emacs19 doesn't give us that handy 8th element!
					(nth 8 result)

					;;					(languide-c-inside-for-control)
					;;					(if (nth 3 result) t nil)
					;;					(nth 4 result)

					))
	    (when in-comment-or-string (goto-char in-comment-or-string))
	    (languide-debug-message 'statement-container "in-comment-or-string=%S" in-comment-or-string)
	    ;;	    (if in-comment-or-string
	    ;;		(forward-char 1))

	    )
	;; could not move out any more; set flag so we terminate now
	(setq in-comment-or-string nil)))
    ;; We are now at the opening brace (or have reached the outermost level, and were there anyway)
    (safe-scan-sexps (point) 1)))

(defun find-next-c-binding-outwards ()
  "Move to the next enclosing binding."
  (let ((binding-pattern "{")
	went)
    (while (and (setq went (outward-once))
		(not (looking-at binding-pattern))))
    (and went
	 (looking-at binding-pattern))))

(defmodal variables-in-scope (c-mode) (whereat)
  "Return the list of variables in scope at WHEREAT."
  (save-excursion
    (goto-char whereat)
    (c-beginning-of-defun)
    (let ((bod (point))
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char whereat)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (find-next-c-binding-outwards)
	(save-excursion
	  (down-list)
	  (skip-to-actual-code)
	  (while (or (looking-at c-binding-regexp-1)
		     (looking-at c-binding-regexp-2)
		     (looking-at c-binding-regexp-3))
	    (let ((vartype (match-string-no-properties 1))
		  (varname (match-string-no-properties 2))
		  (extra (match-string-no-properties 3)))
	      (push (cons varname vartype) variables)
	      (when (stringp extra)
		(message "extra is %s" extra)
		(let ((e 0))
		  (while (string-match ", *\\([a-z][a-z0-9_]*\\)" extra e)
		    (push (cons (match-string-no-properties 1 extra) vartype) variables)		  
		    (setq e (match-end 1))))))
	    (goto-char (match-end 0))
	    (skip-to-actual-code)))
	(backward-char 1))
      ;; now include the funargs
      (goto-char bod)
      (when (and (safe-backward-sexp 1)
		 (safe-down-list 1))
	(c-forward-syntactic-ws)
	(message "bod at %d; trying to find args at %d" bod (point))
	(setq a1 nil a2 nil a3 nil)
	(while (or (setq a1 (looking-at c-arg-regexp-1))
		   (setq a2 (looking-at c-arg-regexp-2))
		   (setq a3 (looking-at c-arg-regexp-3)))
	  (message "%S %S %S" a1 a2 a3)
	  (let ((vartype (match-string-no-properties 1))
		(varname (match-string-no-properties 2)))
	    (push (cons varname vartype) variables))
	  (goto-char (match-end 0))
	  (message "end of match was %d" (point))
	  (c-forward-syntactic-ws)
	  (message "possible start for next match is %d" (point))
	  ))
      variables)))

(defmodal variable-bindings-in-region (c-mode java-mode)  (from to)
  "Return a list of the bindings between FROM and TO.
Each element is a list of:
  name
  type
  scope-begins scope-ends
  initial-value-as-string"
  (save-excursion
    (goto-char from)
    (c-beginning-of-defun)
    (let ((bod (point))
	  (parse-sexp-ignore-comments t)
	  (variables nil))
      (goto-char to)
      ;; (message "Looking for bindings between %d and %d" bod whereat)
      (while (and (find-next-c-binding-outwards)
		  (>= (point) from))
	(save-excursion
	  (let ((scope-begins (point))
		(scope-ends (save-excursion (safe-forward-sexp 1) (point))))
	    (down-list)
	    (skip-to-actual-code)
	    (while (or (looking-at c-binding-regexp-1)
		       (looking-at c-binding-regexp-2)
		       (looking-at c-binding-regexp-3))
	      (let* ((vartype (match-string-no-properties 1))
		     (varname (match-string-no-properties 2))
		     (extra (match-string-no-properties 3))
		     (initial-value-as-string (if extra
						  (substring extra 0 (string-match "," extra))
						"")))
		(push (list varname vartype
			    (point) scope-ends
			    initial-value-as-string)
		      variables)
		(when (stringp extra)
		  (message "extra is %s" extra)
		  (let ((e 0))
		    (while (string-match ", *\\([a-z][a-z0-9_]*\\)" extra e)
		      (push (list (match-string-no-properties 1 extra) vartype
				  (point) scope-ends
				  ""	; todo: get this initial value
				  ) variables)		  
		      (setq e (match-end 1))))))
	      (goto-char (match-end 0))
	      (skip-to-actual-code))))
	(backward-char 1))
      variables)))

(defmodal variable-references-in-region (c-mode java-mode) (from to)
  "Return a list of the variable references between FROM and TO.
Each element is a list of:
  name
  location"
  (let ((results nil)
	(bod (save-excursion (goto-char from) (c-beginning-of-defun) (point))))
    (save-excursion
      (goto-char from)
      (while (re-search-forward "[a-z][a-z0-9_]*" to t)
	(let ((start (match-beginning 0))
	      (name (match-string-no-properties 0))
	      (pps (save-excursion (parse-partial-sexp bod (point)))))
	  (unless (or (fourth pps) (fifth pps))
	    (skip-to-actual-code)
	    (unless (= (char-after) open-bracket)
	      (unless (save-excursion
			(goto-char start)
			(skip-to-actual-code-backwards)
			(or (= (char-before) ?.)
			    (and (= (char-before) ?>)
				 (= (char-before (1- (point))) ?-))))
		  (push (cons name (point)) results)))))))
    (nreverse results)))

(defmodal move-to-enclosing-scope-last-variable-definition (c-mode java-mode perl-mode)
  (&optional allow-conversions)
  "Move to the end of the nearest set of variable bindings.
This is the place at which you would naturally insert a new
variable, allowing for its initial value referring to any
variable already declared.
Optional arguments list names of variables needed in the definition of the new one.
This lets clever implementations put the definition as far out as possible."
  (if (find-next-c-binding-outwards)
      (let ((open (point)))
	(safe-down-list 1)
	(skip-to-actual-code)
	(while (or (looking-at c-binding-regexp-1)
		   (looking-at c-binding-regexp-2)
		   (looking-at c-binding-regexp-3))
	  (goto-char (match-end 0))
	  (skip-to-actual-code))
	(skip-to-actual-code-backwards)
	open)
    nil))

(defmodal insert-variable-declaration (c-mode java-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (unless (save-excursion
	    (let ((here (point)))
	      (back-to-indentation)
	      (= here (point))))
    (newline-and-indent))
  (if type
      (languide-insert type " ")
    (languide-insert "void "))
  (languide-insert name)
  (when initial-value
    (languide-insert " = " initial-value ";")))

(defmodal insert-variable-declaration (perl-mode) (name type initial-value)
  "Insert a definition for a variable called NAME, of TYPE, with INITIAL-VALUE.
Assumes we are at the obvious point to add a new variable.
TYPE and INITIAL-VALUE may be null, but the NAME is required."
  (unless (save-excursion
	    (let ((here (point)))
	      (back-to-indentation)
	      (= here (point))))
    (insert "\n"))
  (languide-insert "my " name)
  (when initial-value
    (languide-insert " = " initial-value ";")))

(defun c-arg-string (arg)
  "Return the string declaring ARG, which is in languide's format, which may be just the name, or (name . type)."
  (cond
   ((stringp arg) arg)			; we hope this one rarely occurs with C
   ((symbolp arg) (symbol-name arg))	; we hope this one rarely occurs with C
   ((consp arg)
    (let ((name (car arg))
	  (type (cdr arg)))
    (concat (if (stringp type)
		type
	      (if (symbolp type)
		  (symbol-name type)
		""))
	    " "
	    (if (stringp name)
		name
	      (if (symbol-name name)
		  (symbol-name name)
		"")))))))

(defmodal insert-function-declaration (c-mode java-mode) (name result-type arglist body &optional docstring)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY."
  (languide-insert "\n" result-type " " name "(")
  (languide-insert (mapconcat 'c-arg-string
		     arglist
		     ", "))
  (languide-insert ")\n")
  (when docstring
    (languide-insert "  /* " docstring " */\n"))
  (let ((start (point)))
    (languide-insert "{\n")
    (languide-insert body)
    (languide-insert "\n}\n")
    (goto-char start)
    (c-indent-exp t)))

(defmodal insert-function-declaration (perl-mode) (name result-type arglist body)
  "Insert a function definition for a function called NAME, returning RESULT-TYPE, taking ARGLIST, and implemented by BODY."
  (languide-insert "sub " name "{\n"
	  "    my (")
  (let ((first t))
    (mapcar (lambda (arg)
	      (unless first (languide-insert " ") (setq first nil))
	      (cond
	       ((stringp arg)
		(languide-insert "$" arg))
	       ((symbolp arg)
		(languide-insert "$" (symbol-name arg)))
	       ((consp arg)
		;; todo: perl-specific clever stuff here
		)
	       ))
	    arglist))
  (languide-insert ") = @_;\n"
	  body
	  "}\n"))

(defconst semicolon (string-to-char ";")
  "Get this out-of-line to avoid confusing indenter when editing functions that use it.")

(defmodal insert-function-call (c-mode java-mode perl-mode) (name arglist)
  "Insert a function call for a function called NAME taking ARGLIST"
  (let ((at-statement-start (save-excursion
			      (skip-to-actual-code-backwards)
			      (memq (char-before) '(?{ semicolon ?})))))
    (languide-insert name "(")
    (languide-insert (mapconcat (function
			(lambda (arg)
			  (if (consp arg)
			      (car arg)
			    arg)))
		       arglist ", "))
    (languide-insert ")")
    (when at-statement-start
      (languide-insert ";\n")
      (save-excursion
	;; (forward-line 1)
	(c-indent-command)))))

(defmodal languide-find-surrounding-call (c-mode java-mode perl-mode) ()
  "Return a list of the function call syntax around point.
Each entry is a cons of start and end positions. For most languages
there will be two or three entries, the function name, the
start-of-call or start-of-args (may be merged with the function name),
and the end-of-call or end-of-args. Separators between arguments could
also be included. The caller should treat these as coming in any
order, and being in any quantity; thus, if using them to modify the
buffer, it is usually necessary to sort them and deal with them in
descending order of character position."
  (save-excursion
    (let ((bod (save-excursion (c-beginning-of-defun) (point))))
      (catch 'found
	(while (and (safe-backward-up-list)
		    (>= (point) bod))
	  (let ((open-bracket (cons (point) (1+ (point)))))
	    (when (save-excursion
		    (backward-sexp)
		    (looking-at "\\(\\s_\\|\\sw\\)+"))
	      (let ((function-name (cons (match-beginning 0) (match-end 0))))
		(forward-sexp)
		(throw 'found (list function-name open-bracket (cons (1- (point)) (point))))))))
	nil))))

;; (defmodal function-arglist-boundaries (c-mode java-mode perl-mode) (&optional where)
;;   "Return a cons of the start and end of the argument list surrounding WHERE,
;; or surrounding point if WHERE is not given.")

(defun find-tag-or-search (tag &optional function)
  "Find TAG, or if cannot be found through the tags table, try to find it anyway."
  (condition-case evar
      (progn
	(find-tag tag)
	t)
    (error
     (let ((old (point)))
       (goto-char (point-min))
       (if (re-search-forward (concat "^[a-z#]+.*\\s-\\*?"
				      tag (if function "\\s-*(" ""))
			      (point-max) t)
	   (progn
	     (beginning-of-line)
	     t)
	 (goto-char old)
	 nil)))))

(defun type-of-c-tag (tag)
  "Find TAG and return its type, or nil if not found."
  (save-window-excursion
    (save-excursion
      (and (find-tag-or-search tag)
	   (let ((start (point)))
	     (search-forward tag)
	     (goto-char (match-beginning 0))
	     (skip-syntax-backward "-")
	     (buffer-substring-no-properties start (point)))))))

(defun type-of-c-function (function)
  "Return the result type of FUNCTION."
  (save-window-excursion
    (save-excursion
      (and (find-tag-or-search function t)
	   (let ((start (point)))
	     (search-forward function)
	     (buffer-substring-no-properties start (match-beginning 0)))))))

(defun type-of-c-variable (variable where)
  "Return the type of VARIABLE."
  (let* ((locals (variables-in-scope where))
	 (as-local (cdr (assoc variable locals))))
    (or as-local
	(type-of-c-tag variable)
	"void")))

(defun remove-leading-expression (str)
  "Return STR with a bracketed expression removed from the front."
  (let ((i 1)
	(n (length str))
	(d 1))
    (while (and (< i n) (> d 0))
      (let ((c (aref str i)))
	(if (= c open-bracket)
	    (setq d (1+ d))
	  (if (= c close-bracket)
	      (setq d (1- d)))))
      (setq i (1+ i)))
    (substring str i)))

(defvar structure-types-cache nil
  "Alist of structure types we have looked at.
Cdrs are alists of member names to types.")

(defun modify-c-type (base-type modifier-op modifier-arg)
  "To BASE-TYPE, apply a MODIFIER-OP and MODIFIER-ARG.
The modifier can be structure accessors, etc."
  (let* ((members-of-type-pair (assoc base-type structure-types-cache)))
    (if members-of-type-pair
	(cdr (assoc modifier-arg (cdr members-of-type-pair)))
      (save-window-excursion
	(save-excursion
	  (find-tag-or-search base-type)
	  (when (looking-at "typedef\\s-+") (goto-char (match-end 0)))
	  (when (looking-at "struct\\s-+") (goto-char (match-end 0)))
	  (forward-sexp)
	  (let ((start (point))
		(end (save-excursion (forward-sexp) (point)))
		(members nil))
	    (while (re-search-forward "\\(\\(?:struct\\s-+\\)?[a-z_]+\\s-*\\*?\\)\\([a-z_]+\\)\\s-*;"
				      end t)
	      (push (cons (match-string-no-properties 2)
			  (match-string-no-properties 1))
		    members))
	    (push (cons base-type members)
		  structure-types-cache)
	    (cdr (assoc modifier-arg members))))))))

(defmodal deduce-expression-type (c-mode java-mode) (value-text where)
  "Given VALUE-TEXT, try to deduce the type of it.
Second arg WHERE gives the position, for context."
  (cond
   ((string-match "[{;}]" value-text)
    (message "\"%s\" appears to be a block, looking for return value" value-text)
    (cond
     ((string-match "return\\s-*;" value-text) "void")
     ((string-match "return\\s-\\([^;]+\\);" value-text)
      (deduce-expression-type (match-string 1 value-text)))
     (t "void")))
   ((string-match "-?[0-9]+\\.[0-9]*" value-text) "float")
   ((string-match "-?[0-9]+" value-text) "int")
   ((string-match "\".*\"" value-text)
    (cond
     ((eq major-mode 'java-mode) "String")
     ((eq major-mode 'c-mode) "char *")))
   ((string-match "\\([a-z][a-z0-9_]*\\)(" value-text)
    (message "\"%s\" appears to be a function call" value-text)
    (let* ((function-type (type-of-c-function (match-string-no-properties 1 value-text)))
	   (remainder (remove-leading-expression (substring value-text (match-end 1))))
	   (is-struct-member (string-match "\\s-*\\([-.>&]+\\)\\s-*\\(.+\\)" remainder))
	   (member-op (if is-struct-member (match-string-no-properties 1 remainder) nil))
	   (member-member (if is-struct-member (match-string-no-properties 2 remainder) nil)))
      (and member-op member-member
	   (modify-c-type function-type member-op member-member))))
   ((string-match "\\([a-z][a-z0-9_]*\\)\\s-*\\([-.>&]+\\)\\s-*\\(.+\\)" value-text)
    (message "match-data %S" (match-data))

    (let* ((base-variable (match-string-no-properties 1 value-text))
	   (member-op (match-string-no-properties 2 value-text))
	   (member-member (match-string-no-properties 3 value-text))
	   (base-variable-base-type (type-of-c-variable base-variable where)))
      (and t (message "\"%s\" appears to be a variable \"%s\" of base type %s with modifier-op \"%s\" and modifier-arg \"%s\""
		      value-text
		      base-variable base-variable-base-type
		      member-op
		      member-member))
      (modify-c-type base-variable-base-type member-op member-member)))
   (t "unknown")))

(defun lisp-operator-to-c (lisp-operator)
  (or (cdr (assoc lisp-operator
		  '((and . "&&")
		    (or . "||")
		    (not . "!"))))
      (symbol-name operator)))

(defmodal add-expression-term (c-mode java-mode perl-mode)
  (operator argument from to)
  "Wrap an expression with OPERATOR and ARGUMENT around the region between FROM and TO."
  (goto-char from)
  (languide-insert "(")
  (goto-char (1+ to))
  (languide-insert (lisp-operator-to-c operator) argument ")"))

(defmodal move-before-defun (c-mode java-mode perl-mode) ()
  "Move to before the current function definition."
  (c-mark-function))			; pollutes mark ring -- sorry

(defun is-under-control-statement (where)
  "Return whether WHERE is the start of the body of a control statement."
  (save-excursion
    (goto-char where)
    (or (and (safe-backward-sexp)
	     (looking-at "else")
	     'if-then-else)
	(and (safe-backward-sexp)
	     (looking-at "\\(if\\)\\|\\(while\\)\\|\\(for\\)")
	     (intern (match-string-no-properties 0))))))

(defmodal languide-region-type (c-mode java-mode perl-mode) (from to)
  "Try to work out what type of thing the code between FROM and TO is.
Results can be things like if-then-body, if-then-else-tail, progn-whole,
while-do-head, defun-body, and so on. If one of these is returned, the
code must be exactly that (apart from leading and trailing
whitespace).
If it is not recognizable as anything in particular, but ends at the
same depth as it starts, and never goes below that depth in between,
that is, is something that could be made into a compound statement or
expression, return t. 
Otherwise return nil."
  (let* ((ppe (parse-partial-sexp from to))
	 (depth (nth 0 ppe))
	 (min-depth (nth 6 ppe))
	 (in-string (nth 3 ppe))
	 (in-comment (nth 4 ppe)))
    ;; (message "depth=%d min-depth=%d" depth min-depth)
    (if (or (/= depth 0)
	    (< min-depth 0)
	    in-string
	    in-comment)
	nil
      (let* ((possible-open-position nil)
	     (preceding-is-open (save-excursion
				  (goto-char from)
				  (skip-to-actual-code-backwards)
				  (backward-char 1)
				  (setq possible-open-position (point))
				  (looking-at (compound-statement-open))))
	     (following-is-close (save-excursion
				   (goto-char to)
				   (skip-to-actual-code)
				   (looking-at (compound-statement-close)))))
	(if (and preceding-is-open
		 following-is-close)
	    (let ((is-under-control
		   (is-under-control-statement possible-open-position)))
	      ;; we have got a whole compound statement
	      (if is-under-control
		  (intern (concat "compound-" (symbol-name is-under-control) "-body"))
		'compound-statement))
	  ;; not a whole compound statement, first see whether it is
	  ;; one statement or several whole statements
	  (let* ((real-start (save-excursion
			       (goto-char from)
			       (skip-to-actual-code)))
		 (real-end (save-excursion
			     (goto-char to)
			     (skip-to-actual-code-backwards)))
		 )
	    (let ((is-single-statement (save-excursion
					 (goto-char real-start)
					 (next-statement-internal 1)
					 (skip-to-actual-code-backwards)
					 (= (point) real-end)))
		  (is-under-bare-control (is-under-control-statement real-start)))
	      (message "single=%S under-bare-control=%S" is-single-statement is-under-bare-control)
	      (if is-single-statement
		  (if is-under-bare-control
		      (intern (concat (symbol-name is-under-bare-control) "-body"))
		    ;; now see whether this statement is the sole contents of a bracketed block
		    (let ((surrounding-open nil))
		      (if (save-excursion
			    (and (progn
				   (goto-char real-start)
				   (skip-to-actual-code-backwards)
				   (backward-char 1)
				   (setq surrounding-open (point))
				   (looking-at (compound-statement-open)))
				 (progn
				   (goto-char real-end)
				   (skip-to-actual-code)
				   (looking-at (compound-statement-close)))))
			  (let ((is-under-bracketed-control (is-under-control-statement surrounding-open)))
			    (message "surrounding-open=%d" surrounding-open)
			    (if is-under-bracketed-control
				(intern (concat (symbol-name is-under-bracketed-control) "-body"))
			      'sole-content-of-block))
			'whole-statement)))
		;; not single statement
		(if is-under-bare-control
		    ;; sequence of statements, but the first one is
		    ;; only the body of a control statement
		    nil
		  ;; now see whether the start and end are whole statements
		  (if (save-excursion
			(and (progn
			       (goto-char (1+ real-start))
			       (beginning-of-statement-internal)
			       (= (point) real-start))
			     (progn
			       (goto-char (1- real-end))
			       (end-of-statement-internal)
			       (= (point) real-end))))
		      'sequence
		    nil))))))))))

(defstatement comment (c-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement comment (java-mode)
  "Comment"
  (head "// *")
  (body "/\\* *" (upto " *$"))
  (tail "$")
  (create (template "// " r n)))

(defstatement comment (perl-mode)
  "Comment"
  (head "# *")
  (body "# *" (upto " *$"))
  (tail "$")
  (create (template "# " r n)))

(defstatement comment (c-mode)
  "Comment"
  (head "/\\* *")
  (body "/\\* *" (upto " *\\*/"))
  (tail " *\\*/")
  (create (template "/* " r " */")))

(defstatement progn (c-mode java-mode perl-mode)
  "Sequential execution statement."
  (head "{")
  (body "{" (statements)))

(defstatement if-then (c-mode java-mode perl-mode)
  "If statement without else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement-contents))
  (add-head (template & > "if (" r ")" n>))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "}"))
  (begin-end "if () {" end "}")
  (begin-end-with-dummy "if (1) {" end "}"))

(defstatement if-then-else (c-mode java-mode perl-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (statement-contents))
  (tail "if" (expression) (statement) "else" (statement-contents))
  (framework (remember "if") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code)
	     (if "{"
		 ((remember "{") (statements) (remember "}"))
	       (statement))
	     (remember "else")
	     (skip-to-actual-code)
	     (continue-if "{")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}"))
  (begin-end "if () {" "} else {}")
  (begin-end-with-dummy "if (1) {" "} else {}"))

(defstatement while-do (c-mode java-mode perl-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (statement-contents))
  (framework (remember "while") (remember "(") (expressions) (remember ")")
	     (skip-to-actual-code) (continue-if "{")
	     (remember "{") (expressions) (remember "}"))
  (create (template & > "while (" p ") {" n>
		    r "}"))
  (begin-end "while () {" "}")
  (begin-end-with-dummy "while (1) {" "}"))

(defstatement do-while (c-mode java-mode perl-mode)
  "Do-While statement."
  (head "do" (statement) "while" (expression))
  (body "do" (statement-contents))
  (create (template & > "do {" r "} while (" p ")" n>)))

(defstatement for (c-mode java-mode perl-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (statement-contents))
  (framework (remember "for") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "for (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement switch (c-mode java-mode)
  "Switch statement"
  (head "switch" (expression-contents))
  (body "switch" (expression) (statement-contents))
  (framework (remember "switch") (remember "(") (expressions) (remember ")")
	     (remember "{") (statements) (remember "}"))
  (create (template & > "switch (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement defun (c-mode java-mode)
  "Function definition"
  (head (upto ")"))
  (body "{" (statements))
  (framework)
  (create (template & >  (p "Function result type: ") (p "Function name: ")
		    "("  (p "Function argument: ") ")"
		    n>
		    "{" n>
		    r
		    "}" n))
  (begin-end "(" ")\n{\n}"))

(defstatement defun (perl-mode)
  "Function definition"
  (head "sub" (expression-contents))
  (body "sub" (expression) (expression-contents))
  (framework (remember "sub") (remember "{") (statements) (remember "}"))
  (create (template & > "sub " (p "Function name: ")
		    n>
		    "{" n>
		    r
		    "}" n))
  (begin-end "sub {" "}"))

(defstatement variable-declaration (perl-mode)
  "My variables"
  (head "my (" (expressions))
  (body "my" (expression) (expressions))
  (framework (remember "my") (remember "(") (expressions) (remember ")"))
  (create (template & > "my (" p ")" n)))

(defstatement variable-declaration (c-mode java-mode)
  "Local variable"
  ;; todo: recognize local variables with and without initial values, as separate statement types
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > " " p ";" n)))

(defstatement assignment (perl-mode c-mode java-mode)
  "Assignment"
  (head "=" (start-of-match) (from-start-of-statement))
  (body "=" (upto ";"))
  (framework (remember "=") (remember ";"))
  (create (template & > (p "Variable: ") " = " r ";")))

(defstatement function-call (perl-mode c-mode java-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (framework (remember "(") (expressions) (remember ")"))
  (create (template & > (p "Function name: ") "(" r ")")))

(defstatement return (c-mode java-mode)
  "Return, with optional result"
  (head "return" (start-of-match) (from-start-of-statement))
  (body "return" (upto ";"))
  (framework (remember "return") (expression) (remember ";"))
  (create (template & > "return" r ";")))

(defstatement and (perl-mode c-mode java-mode)
  "And expression."
  (begin-end "( " " && )")
  (begin-end-with-dummy "( " " && 1)"))

(defstatement or (perl-mode c-mode java-mode)
  "Or expression."
  (begin-end "(" " || )")
  (begin-end-with-dummy "(" " || 0)"))

(defstatement and (perl-mode c-mode java-mode)
  "Not expression."
  (begin-end "(!" ")"))

;; now define the whole statements, systematically
(mapcar (lambda (mode)
	  (let ((statements (get mode 'statements)))
	    (mapcar (lambda (statement)
		      (unless (assoc 'whole (cdr statement))
			(rplacd statement
				(cons '(whole (end-of-statement-internal))
				      (cdr statement)))))
		    statements)))
	'(perl-mode c-mode java-mode))

;;; end of languide-c-like.el
