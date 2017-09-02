;;; posters.el --- querying the posters movie database
;; Copyright (C) 2017 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: movies

;; This file is not part of GNU Emacs.

;; posters.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; posters.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'imdb)

(defun posters-get-image (id)
  (let ((image (imdb-get-image-and-country id t))
	(file (format "/tmp/%s.jpg" id)))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert image)
      (write-region (point-min) (point-max) file))
    file))

(defun posters-make-svg (id text)
  (let* ((file (format "/tmp/%s.jpg" id))
	 (img (create-image file nil nil))
	 (size (image-size img t))
	 (border 80)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) 800))
	 (svg (svg-create (+ image-width border) 800
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 (+ image-width border) 800
		   :fill "red")
    (svg-embed svg file "image/jpeg" nil
	       :width image-width
	       :height 800
	       :x border)
    (svg-text svg text
	      :font-size 50
	      :font-weight "bold"
	      :stroke "white"
	      :fill "white"
	      :font-family "futura"
	      :transform "rotate(270 50 50)"
	      :x -730
	      :y 8)
    svg))

(defun posters-make (id date)
  (posters-get-image id)
  (let ((svg (posters-make-svg id date))
	(file (format "/tmp/%s-poster.png" id)))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "convert"
			   nil nil nil "svg:-" file))
    file))

(provide 'posters)

;;; posters.el ends here
