;;;; buffer-select-hooks.el -- do things when noticing that buffer or mode has changed
;;; Time-stamp: <2004-02-27 10:18:04 john>
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

(provide 'buffer-select-hooks)

(defvar buffer-before-command nil
  "The buffer which was current before the last command.")

(defvar mode-before-command nil
  "The mode which was current before the last command.")

(defun buffer-selection-pre-command-hook ()
  "Function to run before each command, for detecting commands changing the current buffer."
  ;; (message "In buffer-selection-pre-command-hook")
  (setq buffer-before-command (current-buffer)
	mode-before-command major-mode)
  ;; to get round something that is meant to prevent danger -- should I be doing this?
  (add-hook 'post-command-hook 'buffer-selection-post-command-hook)
  )

(defun buffer-selection-post-command-hook ()
  "Function to run after each command, for detecting commands changing the current buffer."
  ;; modes are less specific than buffers, so do them first
  ;; (message "In buffer-selection-post-command-hook")
  (if (not (eq mode-before-command major-mode))
      (progn
	;; (message "Running mode selection hook")
	(run-hook-with-args 'mode-selection-hook mode-before-command)))
  (if (not (eq (current-buffer) buffer-before-command))
      (progn
	;; (message "Running buffer selection hook")
	(run-hook-with-args 'buffer-selection-hook buffer-before-command))))

(add-hook 'pre-command-hook 'buffer-selection-pre-command-hook)
(add-hook 'post-command-hook 'buffer-selection-post-command-hook)

(defvar buffer-selection-hook
  nil
  "Functions to run on selecting a different buffer.
They are given the previous buffer as an argument.")

(defvar mode-selection-hook
  nil
  "Functions to run on finding yourself in a different mode than you were in before the last command.
This could be because of changing buffer, or changing the mode of the same buffer.
They are given the previous mode as an argument.")

(defun mode-selection-in-title-bar (&rest ignore)
  "Indicate the mode in the title bar."
  (set-frame-name (concat "emacs@" (system-name) ":" mode-name)))

;; (add-hook 'mode-selection-hook 'mode-selection-in-title-bar)


