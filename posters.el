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
	 (svg (posters-make-svg-netflix id-file date))
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

(defun posters-make-from-file-netflix (file)
  (let ((svg (posters-make-svg-netflix file "NFLX2019"))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "~/bin/convert"
			   nil (get-buffer-create "*convert*")
			   nil "svg:-" file))
    file))

(defun posters-make-from-file-general (file string &optional color)
  (let ((svg (posters-make-svg-general file string color))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file))))
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


(defun posters-make-svg-netflix (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 600)
	 (font-size 100)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) image-height))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (clear-image-cache)
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height image-height)
    (svg-text svg (format "%s" text)
	      :font-size font-size
	      :stroke "blue"
	      :fill "blue"
	      :stroke-width 14
	      :font-family "Futura"
	      :text-anchor "middle"
	      :transform "rotate(270 0 0)"
	      :y 150
	      :x (- (/ image-height 2)))
    svg))

(defun posters-make-svg-general (file text &optional color)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 900)
	 (font-size 100)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) image-height))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (setq font-size (* (/ (float image-width) 680)
		       45))
    (clear-image-cache)
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height image-height)
    (svg-text svg (format "%s" text)
	      :font-size font-size
	      :font-weight "bold"
	      :stroke (or color "black")
	      :fill (or color "black")
	      :stroke-width 1
	      :font-family "Futura"
	      :y (+ font-size 10)
	      :x 45)
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

(defun posters-find-font-size-for-width (text image-width)
  (loop with prev-height = 10
	for font-size from 10 upto 300
	do (let* ((svg (svg-create (+ image-width 100) image-width
				   :xmlns:xlink "http://www.w3.org/1999/xlink")))
	     (svg-text svg (format "%s" text)
		       :font-size font-size
		       :font-weight "bold"
		       :stroke "black"
		       :fill "black"
		       :stroke-width 1
		       :font-family "Futura"
		       :y (/ image-width 2)
		       :x 0)
	     (let ((file "/tmp/temp.png"))
	       (when (file-exists-p file)
		 (delete-file file))
	       (with-temp-buffer
		 (set-buffer-multibyte nil)
		 (svg-print svg)
		 (call-process-region (point-min) (point-max) "~/bin/convert"
				      nil (get-buffer-create "*convert*")
				      nil "svg:-" file))
	       (call-process "convert" nil nil nil
			     file "-trim" "+repage" "/tmp/crop.png")
	       (with-temp-buffer
		 (call-process "identify" nil (current-buffer)
			       nil "/tmp/crop.png")
		 (let ((size (split-string
			      (nth 2 (split-string (buffer-string)))
			      "x")))
		   (when (>= (string-to-number (car size))
			     image-width)
		     (return (cons (1- font-size) prev-height)))
		   (setq prev-height (string-to-number (cadr size)))))))))

(defun posters-make-svg-big (file text &optional color)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 600)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) image-height))
	 (text-size (posters-find-font-size-for-width text image-width))
	 (font-size (car text-size))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    (clear-image-cache)
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height image-height)
    (svg-text svg (format "%s" text)
	      :font-size font-size
	      :font-weight "bold"
	      :stroke (or color "black")
	      :fill (or color "black")
	      :stroke-width 1
	      :font-family "Futura"
	      :y (+ (/ image-height 2)
		    (/ (cdr text-size) 2))
	      :x 0)
    svg))

(defun posters-make-from-file-big (file string &optional color)
  (let ((svg (posters-make-svg-big file string color))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "~/bin/convert"
			   nil (get-buffer-create "*convert*")
			   nil "svg:-" file))
    file))

(defun posters-find-font-size-for-height (text target-height)
  (loop with prev-width = 10
	for font-size from 10 upto 300
	do (let* ((svg (svg-create 1000 (+ 100 target-height)
				   :xmlns:xlink "http://www.w3.org/1999/xlink")))
	     (svg-text svg (format "%s" text)
		       :font-size font-size
		       :font-weight "bold"
		       :stroke "black"
		       :fill "black"
		       :stroke-width 1
		       :font-family "Futura"
		       :y target-height
		       :x 0)
	     (let ((file "/tmp/temp.png"))
	       (when (file-exists-p file)
		 (delete-file file))
	       (with-temp-buffer
		 (set-buffer-multibyte nil)
		 (svg-print svg)
		 (call-process-region (point-min) (point-max) "~/bin/convert"
				      nil (get-buffer-create "*convert*")
				      nil "svg:-" file))
	       (call-process "convert" nil nil nil
			     file "-trim" "+repage" "/tmp/crop.png")
	       (with-temp-buffer
		 (call-process "identify" nil (current-buffer)
			       nil "/tmp/crop.png")
		 (let ((size (split-string
			      (nth 2 (split-string (buffer-string)))
			      "x")))
		   (when (>= (string-to-number (cadr size))
			     target-height)
		     (return (cons (1- font-size) prev-width)))
		   (setq prev-width (string-to-number (car size)))))))))

;; (find-file (posters-make-from-file-bistro "~/pics/redslur/P1410427.JPG" "Poulet|Rôti l'ami|Louis"))

(defun posters-make-svg-bistro (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 1200)
	 (image-width (round (* (/ (* (car size) 1.0)
				   (cdr size))
				image-height)))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (font-size 400)
	 (y 300))
    (svg-opacity-gradient
     svg 'left-gradient 'linear
     '((0 . "black")
       (40 . "black")))
    (svg-rectangle svg 0 0 image-width (+ image-height 4)
		   :fill "white")
    (svg-embed svg (expand-file-name file)
	       (mailcap-file-name-to-mime-type file)
	       nil
	       :width image-width
	       :height image-height)
    (let ((texts (split-string text "|")))
      (dolist (text texts)
	(setq text (concat text (format "#0%d" (1+ (random 3)))))
	(loop for i from 1 upto 16 by 4
	      do (svg-text svg text
			   :font-size font-size
			   :font-weight "regular"
			   :stroke "black"
			   :stroke-width (format "%s" i)
			   :fill "black"
			   :font-family "JRS"
			   :opacity (format "%.2f" (- 1 (/ (* i 1.0) 16)))
			   :x 50
			   :y y))
	(svg-text svg text		  
		  :font-size font-size
		  :font-weight "regular"
		  :stroke "white"
		  :stroke-width "1"
		  :fill "white"
		  :font-family "JRS"
		  :x 50
		  :y y)
	(incf y (min 300 (/ 1000 (length texts))))))
    svg))

(defun posters-make-from-file-bistro (file string &optional color)
  (let ((svg (posters-make-svg-bistro file string))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region (point-min) (point-max) "~/bin/convert"
			   nil (get-buffer-create "*convert*")
			   nil "svg:-" file))
    file))

(defun posters-change-image-bistro (text)
  (interactive "sTitle: ")
  (if (not (looking-at ".*src=\"\\([^\"]+\\)\""))
      (error "Nothing under point")
    (let* ((old (substring-no-properties (match-string 1)))
	   (new (posters-make-from-file-bistro old text))
	   (edges (window-inside-pixel-edges
		   (get-buffer-window (current-buffer)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert-image
       (create-image new nil nil
	:max-width (truncate (* 0.9 (- (nth 2 edges) (nth 0 edges))))
	:max-height (truncate (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
       (format "<img src=%S>" new)))))

(provide 'posters)

;;; posters.el ends here
