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
  (posters-get-image id)
  (let ((svg (posters-make-svg id date))
	(file (format "/tmp/%s-poster.png" id)))
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

(defun posters-make-svg-century (id text)
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
	      :x -680
	      :y 60)
    svg))

(defun posters-make-svg (id text)
  (let* ((file (format "/tmp/%s.jpg" id))
	 (img (create-image file nil nil))
	 (size (image-size img t))
	 (border 300)
	 (image-height (round (* (/ (* (cdr size) 1.0) (car size)) 1200)))
	 (svg (svg-create 1240 (+ image-height border 40)
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (heading
	  ;;"It's#03 Bergman#01 Time#02"
	  "A#03 Carpenter#01 Winter#02"
	  ))
    (svg-opacity-gradient
     svg 'left-gradient 'linear
     '((0 . "black")
       (40 . "black")))
    (svg-rectangle svg 0 0 1240 (+ image-height border 40 4)
		   :fill "white")	;
    (svg-rectangle svg 20 (+ image-height 20) 1200 border
		   :gradient 'left-gradient)
    (svg-embed svg file "image/jpeg" nil
	       :x 20
	       :y 20
	       :width 1200
	       :height image-height)
    (svg-text svg heading
	      :font-size 160
	      :font-weight "regular"
	      :stroke "red"
	      :stroke-width "8"
	      :fill "black"
	      :font-family "JRS"
	      :text-anchor "left"
	      :x 180
	      :y (+ image-height 80 20))
    (svg-text svg heading
	      :font-size 160
	      :font-weight "regular"
	      :stroke "black"
	      :fill "black"
	      :font-family "JRS"
	      :text-anchor "left"
	      :x 180
	      :y (+ image-height 80 20))
    (svg-text svg text
	      :font-size 120
	      :font-weight "regular"
	      :stroke "black"
	      :stroke-width "4"
	      :fill "black"
	      :font-family "JRS"
	      :text-anchor "middle"
	      :x 620
	      :y (+ image-height 180 20))
    (svg-text svg text
	      :font-size 120
	      :font-weight "regular"
	      :stroke "white"
	      :fill "white"
	      :font-family "JRS"
	      :text-anchor "middle"
	      :x 620
	      :y (+ image-height 180 20))
    (loop for i from 0 upto 50
	  do
	  (svg-rectangle svg (+ i 19) 20 1 (+ image-height border)
			 :opacity (format "%.2f" (- 1 (/ (* i 1.0) 50)))
			 :fill "white")
	  (svg-rectangle svg (- 1220 i) 20 1 (+ image-height border)
			 :opacity (format "%.2f" (- 1 (/ (* i 1.0) 50)))
			 :fill "white")
	  (svg-rectangle svg 20 (+ i 19) 1200 1
			 :opacity (format "%.2f" (- 1 (/ (* i 1.0) 50)))
			 :fill "white")
	  (svg-rectangle svg 20 (- (+ image-height border 20) i) 1200 1
			 :opacity (format "%.2f" (- 1 (/ (* i 1.0) 50)))
			 :fill "white"))
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
