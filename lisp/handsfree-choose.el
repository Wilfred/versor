;;;; handsfree-choose.el -- choose from a list, using pedals or similar
;;; Time-stamp: <2004-01-26 14:31:27 john>
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

(provide 'handsfree-choose)
;; if this is loaded before we are, put it into our menu
(require 'sensible-languages)
(require 'cl)

(defun foo ()
  (interactive)
  (let* ((ones '("a" "b" "c" "d" "e"))
	 (default (car ones))
	 (pairs (mapcar 'list ones)))
    (setq voo (reverse ones))
    (let ((result
	   (completing-read "Choose one of these: " ; prompt
			    pairs	; table
			    nil		; predicate
			    t		; require-match
			    default	; initial-input
			    (cons 'voo
				  (1+ (position default voo))
				  ) ; history
			    )))
      (message "You chose %s" result))))

(defun read-from-minibuffer-with-kill-ring (prompt)
  "Read a string from the minibuffer, with kill-ring as history."
  (read-from-minibuffer prompt
			nil		; initial-contents
			nil		; keymap
			nil		; read
			'kill-ring))

(defvar in-completing-read-with-history-hack nil
  "Non-nil when in completing-read-with-history-hack.
This is used by number commands in my voice software,
to decide whether to throw out to this package, or
alternatively to set the prefix arg.")

(defun completing-read-with-history-hack (prompt
					  history-var default
					  choices-list &optional choices-alist)
  "With PROMPT and using HISTORY-VAR and DEFAULT, choose from CHOICES-LIST."
  (if (and choices-list (null choices-alist))
      (setq choices-alist (mapcar 'list choices-list))
    (when (and choices-alist (null choices-list))
      (setq choices-list (mapcar 'car choices-alist))))
  (set history-var (reverse choices-list))
  ;; (message "Was given choices %S, set history var to %S" choices-list (symbol-value history-var))
  (when (null default) (setq default (car choices-list)))
  (let ((starting (1+ (position default (symbol-value history-var) :test 'string=)))
	(completion-ignore-case t))
    ;; (message "Starting position for %S is %d" default starting)
    (let ((in-completing-read-with-history-hack t))
      (completing-read prompt
		       choices-alist
		       nil		; predicate
		       t		; require-match
		       default		; initial
		       (cons history-var starting)))))

(defvar choices-hack-history nil
  "A history hack variable.")

(defvar choose-singletons-silently t
  "*If non-nil, choose-using-history will, when given a single choice, skip asking the user.")

(defun choose-by-number (n)
  "In the context of choose-using-history, select the Nth item."
  (interactive "p")
  (message "Choosing %d by number from %S" n choices-hack-history)
  (throw 'chosen (nth (- (length choices-hack-history) n 1) choices-hack-history)))

(defun choose-using-history (prompt choices &optional helpstring)
  "With PROMPT, get the user to choose one of CHOICES.
This uses the minibuffer with a history hack like that done in the
tmm package.
If optional HELPSTRING is given, pop that up in a temporary buffer."
  (if (and choose-singletons-silently
	   (null (cdr choices)))
      (car choices)
    (save-window-excursion
      (when (and choices-display-full helpstring)
	(with-output-to-temp-buffer " *Choices*"
	  (princ helpstring)
	  ;; (shrink-window-if-larger-than-buffer (get-buffer-window (get-buffer " *Choices*")))
	  ))
      (let ((result
      (catch 'chosen
	(completing-read-with-history-hack prompt 'choices-hack-history
					   nil
					   choices))))
	(message "Chosen %S" result)
	result))))

(defun choose-value-using-history (prompt choices)
  "With PROMPT, get the user to choose one of CHOICES, which is an alist of strings to values.
This uses the minibuffer with a history hack like that done in the
tmm package."
  (let ((string (completing-read-with-history-hack prompt 'choices-hack-history
				     choices
				     nil
				     nil)))
    (cdr (assoc string choices))))

(defun make-chunk-desc (things)
  "Make a chunk for make-chunk-list."
  (cons (if (null (cdr things))
	    (car things)
	  (format "%s .. %s" (first things) (car (last things))))
	things))

(defun make-full-chunk-descr (things)
  "Make a long description of THINGS for the popup display"
  (let* ((full (mapconcat 'identity (cdr things) ", "))
	 (fulllen (length full))
	 (fw (- (frame-width) 8)))
    (cond
     ((< fulllen fw)
      ;; all fits on one line
      full)
     ((> fulllen (* fw 2))
      ;; more than two lines-worth, use continuation lines
      (concat (substring full 0 fw)
	      " ...\n   ... "
	      (substring full (- fw))))
     (t
      (let ((fulllby2 (/ fulllen 2)))
	(concat (substring full 0 fulllby2)
		"\n    "
		(substring full (- fulllby2))))))))

(defun make-chunk-list (single n)
  "Make a SINGLE list into an alist of N chunks, each labelled with a summary of its contents.
This only makes sense if SINGLE is sorted."
  (let* ((total (length single))
	 (per-chunk (+ (/ total n)
		       ;; compensate for the rounding-down by the integer division above
		       (if (zerop (mod total n)) 0 1)))
	 (chunks nil)
	 (i 0))
    ;; (message "%d total, splitting into %d chunks of %d each which totals %d too many" total n per-chunk (- (* n per-chunk) total))
    (catch 'done
      (while (< i n)
	(push (make-chunk-desc (subseq single 0
				       (min per-chunk (length single)))) chunks)
	;; (when (<= (length single) (- n i)) (setq per-chunk 1))
	(setq single (nthcdr per-chunk single))
	(when (null single) (throw 'done t))
	(incf i)))
    (when (< (* n per-chunk) total)
      (message "make-chunk-desc used overflow!")
      (push (make-chunk-desc single) chunks))
    (nreverse chunks)))

(defvar choices-per-step 6
  "*The number of possible choices per choice step for treewise choosers.")

(defvar choices-display-full t
  "*Whether to display in full the range of choices at each level of a treewise chooser.")

(defun number-items (string-list)
  "Prepend a number to the items of STRING-LIST."
  (let ((i 0)
	(results nil))
    (dolist (string string-list)
      (push (format "%d: %s" i string) results)
      (incf i))
    (nreverse results)))

(defun filter-choices (choices filters)
  "Return just those of CHOICES that match all of FILTERS."
  (let ((result nil))
    (while choices
      (let ((element (car choices))
	    (fs filters)
	    )
	(while (and fs (not (eq fs t)))
	  (setq fs (if (string-match (car fs) element)
		       (cdr fs)
		     t)))
	(if (null fs)
	    (setq result (cons element result)))
	(setq choices (cdr choices))))
    result))

(defun choose-in-steps (prompt choices)
  "With PROMPT, get the user to choose one of CHOICES.
At each stage, up to choices-per-step choices are presented."
  (let* ((all-choices choices)
	 (n (length choices))
	 (max-per-step (if (and (boundp 'vr-mode)
				vr-mode
				(string= vr-mic-state "on"))
			   (if (and (boundp 'vr-choices-per-step)
				    (integerp vr-choices-per-step))
			       vr-choices-per-step
			     (min (- (/ (frame-height) 2) 2)
				  vr-choices-per-step-limit))
			 choices-per-step))
	 (per-step
	  (cond
	   ((<= n max-per-step) n)
	   ((<= n (* max-per-step max-per-step)) (ceiling (sqrt n)))
	   ((<= n (* max-per-step max-per-step max-per-step)) (ceiling (expt (float n) (/ 1.0 3.0))))
	   (t max-per-step)))
	 (previous-levels nil)
	 (extras nil)
	 )
    (while (> n per-step)
      (let* ((chunks (make-chunk-list choices
				      (if t
					  per-step
					(/ n per-step))
				      ))
	     ;; if 'filter is thrown a cons, it is a list of words to filter by
	     (stepkey (catch 'filter
			(choose-using-history prompt
					      (append (mapcar 'car chunks) extras)
					      (mapconcat 'identity
							 (append (number-items
								  (mapcar 'make-full-chunk-descr chunks)) extras)
							 "\n")))))
	(cond
	 ((stringp stepkey)
	  (cond
	   ((string= stepkey "[Back]")
	    (setq choices (car previous-levels)
		  previous-levels (cdr previous-levels)))
	   ((string= stepkey "[Top]")
	    (setq choices all-choices
		  previous-levels nil))
	   (t
	    (let* ((stepval (cdr (assoc stepkey chunks))))
	      (message "Using key %s" stepkey)
	      (setq previous-levels (cons choices previous-levels)
		    choices stepval
		    n (length choices))))))
	 ((consp stepkey)
	  (let ((filtered (filter-choices choices stepkey)))
	    (setq previous-levels (cons choices previous-levels)
		  choices (if filtered filtered choices)
		  n (length choices)))
	  ))
	(if previous-levels
	    (setq extras '("[Back]" "[Top]")))))
    ;; we're now down to sufficiently few choices to take them in a single step.
    (choose-using-history prompt
			  choices
			  (mapconcat 'identity
				     (number-items
				      choices)
				     "\n"))))

(defun make-chunk-tree (single n)
  "Make a tree of chunk lists from SINGLE."
  (message "make-chunk-tree on list of %d" (length single))
;; nesting wrong below, no conditional should be outside the call to make-chunk-list
  (if (> (length single) n)
      (let ((tree (make-chunk-list single n)))
	(if (> (length tree) n)
	(dolist (subtree tree)
	  (rplacd subtree (make-chunk-tree (cdr subtree) n))
	  ))
	tree)
    (mapcar (function
	     (lambda (item)
	       (cons item item)))
	    single)))

(defun chooseable-buffers (limit)
  "return a list of buffers eligible for being chosen."
  (let ((buffers (buffer-list)))
    (if (and (integerp limit)
	     (> (length buffers)
		limit))
	(setq buffers (subseq buffers 0 limit)))
    (delete-if (function
		(lambda (str)
		  (string-match "^ " str)))
	       (mapcar 'buffer-name buffers))))

(defun choose-buffer (prompt &optional limit sort)
  "Choose a buffer, with PROMPT.
With optional LIMIT argument, only looking recently used buffers."
  (let ((buffers (chooseable-buffers limit)))
    (choose-in-steps prompt
		     (if sort
			 (sort buffers 'string<)
		       buffers))))

(defun choose-file-maybe-directory (prompt starting-dir)
  "With PROMPT, choose a file starting from STARTING-DIR."
  (unless (file-directory-p starting-dir) (setq starting-dir (file-name-directory starting-dir)))
  (expand-file-name
   (choose-in-steps prompt (directory-files starting-dir))
   starting-dir))

(defun choose-file (prompt starting-dir)
  "With PROMPT, choose a file starting from STARTING-DIR."
  (let ((file (choose-file-maybe-directory prompt starting-dir)))
    (while (file-directory-p file)
      (setq file (choose-file-maybe-directory prompt file)))
    file))

(defun choose-library (prompt)
  "Handsfree selection of files on emacs load-path."
  (choose-in-steps prompt (sort (mapcar 'car (el-lib-list))
				'string-lessp)))

(defun handsfree-choose-library (library)
  "Find an elisp file on your path."
  (interactive (list (choose-library "Find library file: ")))
  (find-file (elisp-file-more-readable-version (cdr (assoc library (el-lib-list))))))

(defun handsfree-switch-to-buffer (buffer)
  "Switch buffers"
  (interactive (list (choose-buffer "Switch to buffer: " nil t)))
  (switch-to-buffer buffer))

(defun handsfree-switch-to-recent-buffer (buffer)
  "Switch buffers"
  (interactive (list (choose-buffer "Switch to buffer: "
				    (if (and (boundp 'vr-choices-per-step-limit)
					     (integerp vr-choices-per-step-limit))
					vr-choices-per-step-limit
				      choices-per-step)
				    nil)))
  (switch-to-buffer buffer))

(defun handsfree-find-file (file)
  "Find file"
  (interactive (list (choose-file "Find file: " default-directory)))
  (find-file file))

(defun handsfree-choose-email-alias (prompt)
  "With PROMPT, chooose an email alias."
  (when (eq mail-aliases t)
    (setq mail-aliases nil)
    (when (file-exists-p mail-personal-alias-file)
      (build-mail-aliases)))
  (choose-in-steps prompt (sort (remove-if (function
					    (lambda (name)
					      (string-match "-part[0-9]+" name)))
					   (mapcar 'car
						   mail-aliases))
				'string<)))

(defun handsfree-insert-email-alias ()
  "Choose and insert an email alias."
  (interactive)
  (let* ((alias (handsfree-choose-email-alias "Mail alias: "))
	 (expansion (cdr (assoc alias mail-aliases))))
    (message "%s expands to %s" alias expansion)
    (sit-for 1)
    (insert alias)))

(defun handsfree-read-email-addresses (to-prompt &optional just-one)
  "Read email aliases using the handsfree system. Return cons of to-list and cc-list"
  (let* ((recipients (list (handsfree-choose-email-alias to-prompt)))
	 (expansion (cdr (assoc (car recipients) mail-aliases)))
	 (cc nil)
	 (ccq "CC? "))
    (message "%s expands to %s" (car recipients) expansion)
    (sit-for 1)
    (unless just-one
      (while (yes-or-no-p "Further \"to\" recipients? ")
	(let ((this (handsfree-choose-email-alias "Also to: ")))
	  (push this recipients)
	  (message "%s expands to %s" this (cdr (assoc this mail-aliases)))))
      (while (yes-or-no-p ccq)
	(let ((this (handsfree-choose-email-alias "CC to: ")))
	  (push this cc)
	  (message "%s expands to %s" this (cdr (assoc this mail-aliases)))
	  (setq ccq "Further CC? "))))
    (cons (nreverse recipients)
	  (nreverse cc))))

(defun handsfree-mail-to (&optional several)
  "Choose a mail alias, and start a message to them."
  (interactive "P")
  (let* ((both-lists (handsfree-read-email-addresses  "Compose message to: " (not several)))
	 (recipients (car both-lists))
	 (cc (cdr both-lists))
	 (recipient-string (mapconcat 'identity recipients ", "))
	 (cc-string (if cc (mapconcat 'identity cc ", ") ; cc
		      nil))
	 (mail-buffer-name (generate-new-buffer-name
			    (format "mail to %s" recipient-string))))
    (mail nil				; noerase
	  recipient-string		; to
	  nil				; subject
	  nil				; in-reply-to
	  cc-string)
    (rename-buffer mail-buffer-name))
  (mail-subject))

(defun handsfree-vm-forward (&optional several)
  "Forward a message, using the handsfree system to read addresses."
  (interactive "P")
  (let* ((both-lists (handsfree-read-email-addresses "Forward message to: " (not several)))
	 (cc (cdr both-lists)))
    (vm-forward-message)
    (mail-to) (insert (mapconcat 'identity (car both-lists) ", "))
    (when cc
      (mail-cc) (insert (mapconcat 'identity cc ", ")))))

(defun handsfree-choose-web-directory (prompt)
  "Get the user to select a web directory"
  (choose-in-steps prompt (sort (mapcar 'car (web-directories)) 'string<)))

(defun handsfree-raise-web-directory ()
  "Choose a web directory, and bring it to the top of the buffer list."
  (interactive)
  (let* ((web-dir (handsfree-choose-web-directory "Raise web directory: ")))
    (raise-web-directory web-dir)))

(defun handsfree-choose-context (prompt)
  "Get the user to select a context"
  (choose-in-steps prompt (sort (mapcar 'car context-contexts) 'string<)))

(defun handsfree-load-context ()
  "Choose a context, and bring it to the top of the buffer list."
  (interactive)
  (require 'contexts)
  (let* ((context (handsfree-choose-context "Switch to context: ")))
    (context-load context)))

(defun handsfree-choose-screen-setup ()
  "Get the user to select a screen setup."
  (choose-in-steps "Screen setup: " (sort (mapcar 'car screen-setups) 'string<)))

(defun handsfree-select-screen-setup ()
  (interactive)
  (require 'screen-setups)
  (use-screen-setup (handsfree-choose-screen-setup)))

(pushnew 'handsfree-choose-menu menu-bar-final-items)
(defvar handsfree-choose-menu (make-sparse-keymap "Handsfree choose"))
(define-key global-map [menu-bar handsfree-choose] (cons "Handsfree choose" handsfree-choose-menu))

(define-key handsfree-choose-menu [find-file]
  '("Find File" . handsfree-find-file))

(define-key handsfree-choose-menu [send-message]
  '("Send message" . handsfree-mail-to))

(define-key handsfree-choose-menu [raise-web-directory]
  '("Raise web directory" . handsfree-raise-web-directory))

(define-key handsfree-choose-menu [select-screen-setup]
  '("Select screen setup" . handsfree-select-screen-setup))

(if (featurep 'sensible-languages)
    (define-key handsfree-choose-menu [statement-type]
      '("Statement type" . statement-type)))

(define-key handsfree-choose-menu [handsfree-load-context]
  '("Load context" . handsfree-load-context))

(define-key handsfree-choose-menu [ handsfree-choose-library ]
  '("Find library file" . handsfree-choose-library))

(define-key handsfree-choose-menu [switch-to-buffer]
  '("Switch to buffer" . handsfree-switch-to-buffer))

(define-key handsfree-choose-menu [switch-to-recent-buffer]
  '("Switch to recent buffer" . handsfree-switch-to-recent-buffer))

;;;; end of handsfree-choose.el
