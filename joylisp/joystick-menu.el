;;; joystick-menu.el --- drive the menu system from a joystick

;; Copyright (C) 2007  John Sturdy

;; Author: John Sturdy <jcgs@hosea>
;; Keywords: hardware

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This lets you pop up the menu, and navigate it using a joystick or
;; gamepad.

;;; Code:

(require 'joystick)

(defvar joystick-tmm-old-map nil
  "The minibuffer map before we got at it to add the joystick.")

(defvar joystick-tmm-map
  (let ((new-map (make-sparse-keymap)))
    (suppress-keymap new-map t)
    (define-key new-map [ BaBt2-Hat0Y-next ] 'next-history-element)
    (define-key new-map [ BaBt2-Hat0Y-previous ] 'previous-history-element)
    (define-key new-map [ BaBt2-Hat0X-next ] 'next-history-element)
    (define-key new-map [ BaBt2-Hat0X-previous ] 'previous-history-element)
    ;; for consistency, as I'm using Trigger as Enter in various ways:
    (define-key new-map [ BaBt2-Trigger-up ] 'minibuffer-complete-and-exit)
    ;; for being near the relevant modifier:
    (define-key new-map [ BaBt2-BaseBtn-up ] 'minibuffer-complete-and-exit)
    ;; not sure whether I need these
;;     (define-key new-map [ Hat0Y-next ] 'next-history-element)
;;     (define-key new-map [ Hat0Y-previous ] 'previous-history-element)
;;     (define-key new-map [ Hat0X-next ] 'next-history-element)
;;     (define-key new-map [ Hat0X-previous ] 'previous-history-element)
    (define-key new-map [ Trigger-up ] 'minibuffer-complete-and-exit)
    (define-key new-map [ BaseBtn2-release ] 'minibuffer-complete-and-exit)
    new-map)
  "Keymap for running tmm from the joystick.")

(defun joystick-menu-tmm-minibuffer-setup-function ()
  "Make minibuffer arrangements for the joystick to work in tmm."
  (setq joystick-tmm-old-map (current-local-map))
  (use-local-map (append joystick-tmm-map joystick-tmm-old-map))
  (message "local map now %S" (current-local-map)))

(defun joystick-menu-tmm-minibuffer-takedown-function ()
  "Cancel minibuffer arrangements for the joystick to work in tmm."
  (use-local-map joystick-tmm-old-map))

 (defun joystick-menu ()
   "Execute a command from the menubar system, using a joystick."
   (interactive)
   (if (and nil menu-bar-mode)
       ;; this is done in the C code, so I suspect we can't use it,
       ;; hence the wistful "and nil" above
       (menu-bar-open)
     (unwind-protect
	 (progn
	   (add-hook 'minibuffer-setup-hook
		     'joystick-menu-tmm-minibuffer-setup-function)
	   (add-hook 'minibuffer-exit-hook
		     'joystick-menu-tmm-minibuffer-takedown-function)
	   (tmm-menubar))
       (remove-hook 'minibuffer-setup-hook
		    'joystick-menu-tmm-minibuffer-setup-function)
       (remove-hook 'minibuffer-exit-hook
		    'joystick-menu-tmm-minibuffer-takedown-function))))

(provide 'joystick-menu)

;;; joystick-menu.el ends here
