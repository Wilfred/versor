;;;; handsfree.el -- operate emacs (almost) entirely through some footswitches
;;; Time-stamp: <2006-08-02 12:18:56 john>
;;
;; Copyright (C) 2004, 2006  John C. G. Sturdy
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

(require 'pedals)
(require 'flexi-choose)
(require 'handsfree-menus)
(require 'handsfree-tools-menus)

(when (null yes-or-no-p-history)
  (setq yes-or-no-p-history '("yes" "no")))

(provide 'handsfree)

;;; end of handsfree.el



