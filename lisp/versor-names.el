;;;; versor-names.el -- part of dimensional navigation
;;; Time-stamp: <2004-01-23 16:43:42 john>
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

;;; This is not needed for using versor just through the cursor keys
;;; or pedals, but is used by versor-voice and versor-menu

(provide 'versor-names)
(require 'versor)

(defun versor:meta-level-names ()
  "Return an alist of the meta-level names, consed with the numbers."
  (let ((result nil))
    (dotimes (i (length moves-moves))
      (if (> i 0)
	  (push (cons (aref (aref moves-moves i) 0)
		      i)
		result)))
    result))

(defun versor:level-names ()
  "Return an alist of the level names for the current meta-level, consed with the numbers."
  (let* ((result nil)
	 (meta-level (versor:current-meta-level))
	 (n (length meta-level))
	 (i 1)
	 )
    (while (< i n)
      (push (cons (car (aref meta-level i))
		  i)
	    result)
      (incf i))
    result))

(defun versor:find-meta-level-by-name (name)
  "Return the index of the meta-level called NAME."
  (let* ((meta-level-index (1- (length moves-moves))))
    (catch 'found
      (while (> meta-level-index 0)
	(when (string= name (aref (aref moves-moves meta-level-index) 0))
	  (throw 'found meta-level-index))
	(decf meta-level-index))
      nil)))

(defun versor:find-level-by-double-name (meta-name name)
  "Return the index of the level called META-NAME NAME as a cons of (meta-level . level)."
  (let ((meta-level-index (versor:find-meta-level-by-name meta-name)))
    (if meta-level-index
	(let ((level-index (1- (length (aref moves-moves meta-level-index)))))
	  (catch 'found
	    (while (>= level-index 0)
	      (when (string= name (car (aref (aref moves-moves meta-level-index) level-index)))
		(throw 'found (cons meta-level-index level-index)))
	      (decf level-index))
	    nil))
      nil)))

(defun versor:find-level-by-single-name (name)
  "Return the index of the level called NAME as a cons of (meta-level . level).
If this occurs in several meta-levels, including the current one, use the occurrece
in the caurrent one."
  (let* ((meta-level-index versor:meta-level))
    (catch 'found
      (while t
	(let* ((meta-level (aref moves-moves meta-level-index))
	       (level-index (1- (length meta-level))))
	  (while (> level-index 0)
	    (when (string= name
			   (car (aref meta-level level-index)))
	      (throw 'found (cons meta-level-index level-index)))
	    (decf level-index)))
	(incf meta-level-index)
	(when (= meta-level-index (length moves-moves))
	  (setq meta-level-index 1))
	(when (= meta-level-index versor:meta-level)
	  ;; gone right round
	  (throw 'found nil)))
      nil)))


(defvar versor-meta-history-hack-var nil
  "History hack.")

(defvar versor-history-hack-var nil
  "History hack.")

(defun versor:select-named-meta-level (name)
  "Set the current meta-level to NAME."
  (interactive
   (list
    (completing-read-with-history-hack
     "Meta-level: "
     'versor-meta-history-hack-var
     versor:current-meta-level-name
     (mapcar 'car (versor:meta-level-names)))))
  (setq versor:meta-level (versor:find-meta-level-by-name name))
  (versor::trim-meta-level)
  (versor::trim-level)
  (versor:set-status-display))

(defun versor:select-named-level (name)
  "Set the current level to NAME."
  (interactive
   (list
    (completing-read-with-history-hack
     "Level: "
     'versor-history-hack-var
     versor:current-level-name
     (mapcar 'car (versor:level-names)))))
  (let ((level-pair (versor:find-level-by-single-name name)))
    (if level-pair
	(progn
	  (setq versor:old-level versor:level
		versor:meta-level (car level-pair)
		versor:level (cdr level-pair))
	  (versor:set-status-display))
      (error "Could not find versor level named %s" name))))

(defun versor:all-level-names (&optional include-meta)
  "Return the list of level names.
With non-nil arg, include meta-level names."
  (let ((names nil))
    (dotimes (meta-level-index (length moves-moves))
      (if (> meta-level-index 0)
	  (let ((meta-level (aref moves-moves meta-level-index)))
	    (dotimes (level-index (length meta-level))
	      (if (zerop level-index)
		  (when include-meta (pushnew (aref meta-level 0) names :test 'string=))
		(pushnew (car (aref meta-level level-index)) names :test 'string=))))))
    (sort names 'string<)))

;;; end of versor-names.el