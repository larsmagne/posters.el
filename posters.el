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

;; To test:
;; (find-file (posters-make "tt0084787" "The Thing"))

;;; Code:

(require 'imdb)

(defun posters-make (id date)
  "Create an image based on the poster for ID with a sidebar of DATE.
ID is the imdb movie ID, and DATE can be any string."
  (let* ((id-file (posters-get-image id))
	 (svg (posters-make-svg id-file date))
	 (file (format "/tmp/%s-poster.jpg" id)))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "~/bin/convert"
			   nil (get-buffer-create "*convert*")
			   nil "svg:-" file))
    file))

(defun posters-make-from-file (file)
  (let ((svg (posters-make-svg file ""))
	(file (format "/tmp/%s-poster.png" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "~/bin/convert"
			   nil (get-buffer-create "*convert*")
			   nil "svg:-" file))
    file))

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

(defun posters-make-svg-century (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (border 80)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) 800))
	 (svg (svg-create (+ image-width border) 800
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-rectangle svg 0 0 (+ image-width border) 800
		   :fill "red")
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
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
	      :x -680
	      :y 60)
    svg))

(defun posters-make-svg (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 1200)
	 (image-width (round (* (/ (* (car size) 1.0)
				   (cdr size))
				image-height)))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (font-size 200)
	 (heading "87 Bergman#01 Things#02"))
    (svg-opacity-gradient
     svg 'left-gradient 'linear
     '((0 . "black")
       (40 . "black")))
    (svg-rectangle svg 0 0 image-width (+ image-height 4)
		   :fill "white")
    (svg-embed svg (expand-file-name file)
	       (if (string-match "png$" file)
		   "image/png"
		 "image/jpeg")
	       nil
	       :width image-width
	       :height image-height)
    (loop for i from 1 upto 16 by 4
	  do (svg-text svg heading
		       :font-size font-size
		       :font-weight "regular"
		       :stroke "black"
		       :stroke-width (format "%s" i)
		       :fill "black"
		       :font-family "JRS"
		       :transform "rotate(270 50 50)"
		       :opacity (format "%.2f" (- 1 (/ (* i 1.0) 16)))
		       :x (+ (- image-height) 150)
		       :y 180))
    (svg-text svg heading
	      :font-size font-size
	      :font-weight "regular"
	      :stroke "white"
	      :stroke-width "1"
	      :fill "white"
	      :font-family "JRS"
	      :transform "rotate(270 50 50)"
	      :x (+ (- image-height) 150)
	      :y 180)
    svg))

(defun svg-opacity-gradient (svg id type stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.  STOPS is a list of percentage/color
pairs."
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-opacity . ,(cdr stop)))))
     stops))))

(provide 'posters)

;;; posters.el ends here
