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

(defun posters-make-svg (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) 800))
	 (svg (svg-create image-width  800
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height 800
	       :x 0)
    (svg-text svg (format "%s" text)
	      :font-size 50
	      :font-weight "bold"
	      :stroke "black"
	      :fill "black"
	      :stroke-width 8
	      :font-family "Brush ATF"
	      :text-anchor "middle"
	      :y 50
	      :x (/ image-width 2))
    (when t
      (svg-text svg (format "%s" text)
		:font-size 50
		:font-weight "bold"
		:stroke "white"
		:stroke-width 0
		:fill "white"
		:font-family "Brush ATF"
		:text-anchor "middle"
		:y 51
		:x (- (/ image-width 2) 2)))
    (svg-text svg "Decade!"
	      :font-size 50
	      :font-weight "bold"
	      :stroke "black"
	      :fill "black"
	      :stroke-width 8
	      :font-family "Brush ATF"
	      :text-anchor "middle"
	      :y (- (cdr size) 215)
	      :x (/ image-width 2))
    (when t
      (svg-text svg "Decade!"
		:font-size 50
		:font-weight "bold"
		:stroke "white"
		:stroke-width 0
		:fill "white"
		:font-family "Brush ATF"
		:text-anchor "middle"
		:y (- (cdr size) 214)
		:x (- (/ image-width 2) 2)))
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
