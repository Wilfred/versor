;;;; versor-alter-item.el -- choose possible value for the current item
;;; Time-stamp: <2004-12-02 15:27:15 john>
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

(provide 'versor-alter-item)

(defun versor-alter-item-next ()
  "Replace the current item with the next possible value."
  (interactive)
)

(defun versor-alter-item-prev ()
  "Replace the current item with the previous possible value."
  (interactive)
)

(defun versor-alter-item-over-next ()
  "Replace the current item with the corresponding value from the next range."
  (interactive)
)

(defun versor-alter-item-over-prev ()
  "Replace the current item with the corresponding value from the previous range."
  (interactive)
)

;;; end of versor-alter-item.el
