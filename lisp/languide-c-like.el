;;;; languide-c-like.el -- C, java, perl definitions for language-guided editing
;;; Time-stamp: <2004-02-24 14:46:30 john>
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

(defun skip-to-actual-code (&optional limit)
  "Skip forward, over any whitespace or comments, to the next actual code.
This assumes that we start in actual code too.
LIMIT, if given, limits the movement."
  (interactive)
  (parse-partial-sexp (point) (if limit limit (point-max))
		      0			; target-depth
		      t			; stop-before
		      nil		; state
		      nil		; stop-comment
		      )
  (point))

(defun languide-c-back-to-possible-ender (bod)
  "Move point back to be at something that might end a C statement,
and is outside a comment or string, relative to BOD.
BOD is Beginning Of Defun, which is taken to be not in a comment or string."
  (let ((in-comment-or-string t))
    ;; keep looking for the possible start of a statement, and checking that
    ;; it is not part of a comment or string
    (while in-comment-or-string
      (cond
       ((and nil (looking-at "[a-z_]"))
	(skip-syntax-backward "w_")
	)
       (t
	(re-search-backward "[{;}]" (point-min) t) ; leaves point at start of match
	(message "Found a \"%s\" at %d" (match-string 0) (point))
	(let ((result (save-excursion (parse-partial-sexp bod (point)
							  0 ; target-depth
							  nil ; stop-before
							  nil ; state
							  nil ; stop-comment
							  ))))
	  (message "parse-partial-sexp returned %S" result)
	  (setq in-comment-or-string (or
				      ;; emacs19 doesn't give us that handy 8th element!
				      (nth 8 result)
				      (if (nth 3 result) t nil)	; only want a number in in-comment-or-string if it tells us a character position; this one gives us a character code
				      (nth 4 result)
				      (languide-c-inside-for-control)
				      ))
	  (message "in-comment-or-string=%S" in-comment-or-string)
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

(defmodal beginning-of-statement-internal (c-mode perl-mode) (n)
  "Move to the beginning of a C or Perl statement."
  ;; to do to this: if we were already before a statement, and the first non-comment
  ;; non-whitespace thing we find on the way backwards is a semicolon, we should go
  ;; back before the semicolon and carry on looking, otherwise this won't work for use
  ;; as "previous-statement"
  (interactive "p")
  (let ((old (point))
	;; get beginning of defun, so we can use parse-partial-sexp to
	;; see whether we have landed in a string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (message "Starting beginning-of-statement-internal-c-perl at %d, with beginning-of-defun at %d" old bod)
    ;; (if (looking-at "{") (backward-char 1))
    (while (> n 0)
      (languide-c-back-to-possible-ender bod)
      (let ((possible-ender (point)))

	(message "Countdown %d; now at %d: \"%s\", which is not in a comment or string" n (point) (buffer-substring (point) (+ (point) 20)))

	;; We've now found a statement delimiter, and checked that it is a
	;; real one, and not part of a string or comment. Now we might make
	;; some adjustments, then finally move over any whitespace or comments
	;; leading in to the actual statement.

	(cond
	 ((looking-at "{")
	  (message "found open")
	  (setq n 0)  ; can't go outside container, so stop right here
	  (forward-char 1))
	 ((looking-at "}")
	  (forward-char 1)
	  (let* ((close (point))
		 (following-code (skip-to-actual-code)))
	    (message "From old at %d, found close at %d, following code at %d is \"%s\"" old close following-code (buffer-substring following-code (+ following-code 20)))
	    (cond
	     ;; first two cases are those where a keyword can follow a closing brace
	     ;; and still be within a statement
	     ((looking-at "\\<while\\>")
	      (message "Got \"while\", looking back to see which kind")
	      ;; might be
	      ;;   do { ... } * while ( ... )
	      ;; or
	      ;;   { ... } * while ( ... ) { ... }
	      ;; and so must go back to check for the "do"
	    ;;;;;;;;;;;;;;;; will only work with compound statements for now, there are
	    ;;;;;;;;;;;;;;;; several of these, and I should probably make them recurse
	    ;;;;;;;;;;;;;;;; on this routine as needed!!!!!!!!!!!!!!!!
	      (goto-char (safe-scan-sexps (point) -2)) 
	      (message "That get us to \"%s\"" (buffer-substring (point) (+ (point) 20)))
	      (unless (looking-at "\\<do\\>")
		(message "was not \"do\"")
		(goto-char following-code))
	      (decf n))
	     ((looking-at "\\<else\\>")
	      ;;   if ( ... ) { ... } * else { ... }
	      ;; and so must go back
	      (message "Got \"else\" following closing brace, so going back")
	      (goto-char (safe-scan-sexps (point) -3))
	      (message "That got us to \"%s\"" (buffer-substring (point) (+ (point) 20)))
	      (decf n))

	     ;; having dealt with both the "{ ... } <keyword>" cases,
	     ;; now see whether we started inside a statement that follows a closing brace
	     ((> old following-code)
	      (message "We started inside the statement following a closing brace, so go to the start of that statement")
	      (goto-char following-code)
	      (decf n))

	     ;; next, try going back 2 sexps (really should be a statement and a keyword)
	     ;; and seeing if that is an else
	     ((progn
		(goto-char (safe-scan-sexps close -2))
		(looking-at "\\<else\\>"))
	      (message "Got else")
	      (goto-char (safe-scan-sexps (point) -3))
	      (decf n)
	      )

	     ;; if it wasn't an else, try going back another sexp
	     ((progn
		(goto-char (safe-scan-sexps (point) -1))
		(looking-at "\\<\\(if\\)\\|\\(while\\)\\|\\(for\\)\\|\\(until\\)\\>"))
	      (message "Got if/while/for/until")
	      (decf n)
	      )

	     (t
	      ;; Go back and see what was before the brace; could be
	      ;;   if ( ... ) { ... }
	      ;;   while ( ... ) { ... }
	      ;;   until ( ... ) { ... }
	      ;;   for ( ... ) { ... }
	      ;; or it could just be a free-standing code block
	      (message "Other case of closing brace, at \"%s\"" (buffer-substring (point) (+ (point) 20)))
	     

	      (message "Other case; going back to look before the brace... got \"%s\"" (buffer-substring (point) (+ (point) 20)))
	      (cond
		 
		
	       (t
		(message "Before the brace was not if/while/for/until/else; assuming plain block")
		(goto-char (safe-scan-sexps close -1))
		(decf n)))))))

	 ((looking-at ";")
	  (message "found semicolon at %d" (point))
	  (let ((following-code (skip-to-actual-code)))
	    (if (> old following-code)
		(progn
		  (message "already after following code, here we are")
		  (goto-char following-code)
		  (decf n))
	      (progn
		(message "already before following code, go back another")
		(goto-char (- possible-ender 1)) ; need to do more than this!!!!!!!!!!!!!!!!
		)
	      )
	    )
	  (when (save-excursion	; this is old version, is it right????????????????
		  (parse-partial-sexp (point) old nil t)
		  (= (point) old))
	    (message "no code between it and where we were (%d); am now at %d" old (point))
	    (backward-char 1)
	    (incf n)
	    )))))

    ;; now we're at a real statement delimiter
    (skip-to-actual-code old)))

(defmodal end-of-statement-internal (c-mode perl-mode) (n)
  "Move to the end of a C or Perl statement."
  ;; (re-search-forward "[{;}]" (point-max) t)
  (let ((old (point))
	;; get beginning of defun, so we can see whether we have landed in a
	;; string or comment
	(bod (save-excursion
	       (beginning-of-defun 1)
	       (point))))
    (message "Starting end-of-statement-internal at %d, with beginning-of-defun at %d" old bod)
    ;; (if (looking-at "{") (backward-char 1))
    (while (> n 0)
      (message "Countdown %d" n)
      (let ((in-comment-or-string t))
	;; keep looking for the possible start of a statement, and checking that
	;; it is not part of a comment or string
	(while in-comment-or-string
	  (re-search-forward "[{;}]" (point-max) t) ; leaves point at end of match
	  (message "Found a \"%s\" at %d" (match-string 0) (point))
	  (let ((result (save-excursion (parse-partial-sexp bod (point)
							    0 ; target-depth
							    nil	; stop-before
							    nil	; state
							    nil	; stop-comment
							    ))))
	    (message "parse-partial-sexp returned %S" result)
	    (setq in-comment-or-string (or
					;; emacs19 doesn't give us that handy 8th element!
					(nth 8 result)
					(languide-c-inside-for-control)
					(if (nth 3 result) t nil)
					(nth 4 result)
					))
	    (message "in-comment-or-string=%S" in-comment-or-string)
	    (if in-comment-or-string
		(forward-char 1)))))

      (message "Now at %d=%c, which is not in a comment or string; the latest match, which is \"%s\", starts at %d"
	       (point) (char-after (point)) (match-string 0) (match-beginning 0))
      (cond
       ((= (char-after (1- (point))) ?{)
	(message "Found block start")
	(backward-char 1)
	(forward-sexp 1)))
      (cond
       ((save-excursion
	  (skip-to-actual-code)
	  (looking-at "else"))
	(message "Found ELSE")
	(forward-sexp 2)))
      (decf n)))
  (skip-to-actual-code))

(defmodal identify-statement (c-mode) (default)
  "Identify the current statement, or return DEFAULT.
We must be at the start of the statement already, otherwise
this does not have to work."
  (cond
   ((looking-at "\\(do\\)\\|\\(for\\)\\|\\(while\\)\\|\\(if\\)\\|\\(switch\\)\\|\\(continue\\)\\|\\(default\\)\\|\\(case\\)")
    (message "identify-statement-c-mode found %s" (match-string 0))
    (let ((keyword-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (cond
       ((string= keyword-string "if")
	(save-excursion
	  (forward-sexp 3)
	  (skip-to-actual-code)
	  (if (looking-at "else")
	      'if-then-else
	    'if-then)))
       ((string= keyword-string "for") 'for)
       ((string= keyword-string "while") 'while)
       ((string= keyword-string "switch") 'switch)
       ((string= keyword-string "continue") 'continue)
       ((string= keyword-string "case") 'case)
       ((string= keyword-string "default") 'default)
       ((string= keyword-string "do") 'do)
       (t nil)
       )
      )
    )
   ((save-excursion
      (forward-sexp 1)
      (looking-at " *=[-+*/]? *"))
    'assignment)
   ((save-excursion
      (forward-sexp 1)
      (looking-at "\\(++\\)\\|\\(--\\)"))
    'assignment)
   ((save-excursion
      (forward-sexp 1)
      (looking-at " *("))
    'function-call)
   ((save-excursion
      (forward-sexp 2)
      (looking-at " *[=;] *"))
    'variable-declaration)
   ((save-excursion
      (looking-at "[^;()=]+;"))
    'variable-declaration)
   ((eq (char-after (point)) ?{)
    'progn)
   (t default)
   ))

(defmodal insert-compound-statement-open (c-mode perl-mode) ()
  "Insert a block start."
  (insert "{ "))

(defmodal insert-compound-statement-close (c-mode perl-mode) ()
  "Insert a block end."
  (insert "}"))

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

(defstatement if-then (c-mode java-mode perl-mode)
  "If statement without else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (expression-contents))
  (create (template & > "if (" p ") {" n>
		    r "}")))

(defstatement if-then-else (c-mode java-mode perl-mode)
  "If statement with else clause."
  (head "if" (expression-contents))
  (body "if" (expression) (expression-contents))
  (tail "if" (expression) (expression) "else" (expression-contents))
  (create (template & > "if (" p ") {" n>
		    r "} else {"n>
		    p "}")))

(defstatement while-do (c-mode java-mode perl-mode)
  "While statement."
  (head "while" (expression-contents))
  (body "while" (expression) (expression-contents))
  (create (template & > "while (" p ") {" n>
		    r "}")))

(defstatement do-while (c-mode java-mode perl-mode)
  "Do-While statement."
  (head "do" (expression-contents) "while" (expression))
  (body "do" (expression-contents))
  (create (template & > "do {" r "} while (" p ")" n>)))

(defstatement for (c-mode java-mode perl-mode)
  "For statement."
  (head "for" (expression-contents))
  (body "for" (expression) (expression-contents))
  (create (template & > "while (" p ";" p ";" p ") {" n>
		    r "}")))

(defstatement defun (perl-mode)
  "Function definition"
  (head "sub" (expression-contents))
  (body "sub" (expression) (expression-contents))
  (create (template & > "sub " (p "Function name: ")
		    n>
		    "{" n>
		    r
		    "}" n)))

(defstatement variable-declaration (perl-mode)
  "My variables"
  (head "my (" (expressions))
  (body "my" (expression) (expressions))
  (create (template & > "my (" p ")" n)))

(defstatement assignment (perl-mode c-mode java-mode)
  "Assignment"
  (head "=" (start-of-match) (preceding-expression))
  (body "=" (expression))
  (create (template & > (p "Variable: ") " = " r ";")))

(defstatement function-call (perl-mode c-mode java-mode)
  "Function call"
  (head (expression))
  (body (expression) (expression-contents))
  (create (template & > (p "Function name: ") "(" r ")")))
