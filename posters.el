;;; posters.el --- querying the posters movie database  -*- lexical-binding: t -*-
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
(require 'seq)

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

(defun posters-make-from-file-director (file string &optional color)
  (posters--make-and-save #'posters-make-svg-director file string color))

(defun posters-change-image-qc (string)
  (interactive "sString: ")
  (setq string (upcase string))
  (if (not (looking-at ".*src=\"\\([^\"]+\\)\""))
      (error "Nothing under point")
    (let* ((old (substring-no-properties (match-string 1)))
	   (new (posters-make-from-file-qc old string))
	   (edges (window-inside-pixel-edges
		   (get-buffer-window (current-buffer)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert-image
       (create-image
	new nil nil
	:max-width (truncate (* 0.9 (- (nth 2 edges) (nth 0 edges))))
	:max-height (truncate (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
       (format "<img src=%S>" new)))))

(defun posters-make-from-file-qc (file string &optional color)
  (posters--make-and-save #'posters-make-svg-qc file string color))

(defun posters--make-and-save (func file string color)
  (let ((svg (funcall func file string color))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (when (file-exists-p "/tmp/poster.svg")
	(delete-file "/tmp/poster.svg"))
      (when (file-exists-p "/tmp/poster.svg")
	(delete-file "/tmp/poster.svg"))
      (write-region (point-min) (point-max) "/tmp/poster.svg"))
    (call-process "rsvg-convert"
		  nil (get-buffer-create "*convert*") nil
		  "/tmp/poster.svg" "-o" file)
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

(defun posters-make-svg-director (file text &optional color)
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
	      :x (+ 10 (/ font-size 5)))
    svg))

(defun posters-make-svg-qc (file text)
  (let* ((img (create-image file nil nil))
	 (size (image-size img t))
	 (image-height 900)
	 (font-size 100)
	 (image-width (* (/ (* (car size) 1.0) (cdr size)) image-height))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink"))
	 (colours '("#e40303" "#ff8c00" "#ffed00"
		    "#008026" "#004dff" "#750787"))
	 (clip (svg-clip-path svg :id "text")))
    (setq font-size (* (/ (float image-width) 680)
		       45))
    (clear-image-cache)
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height image-height)
    (svg-text clip (format "%s" text)
	      :font-size font-size
	      :font-weight "bold"
	      :font-family "Futura"
	      :y (+ font-size 10)
	      :x (+ 10 (/ font-size 5)))
    (dotimes (i 50)
      (let ((step (/ image-width 20.0)))
	(svg-rectangle svg (+ (* i step) (+ 18 (/ font-size 5))) 0
		       step image-width
		       :clip-path "url(#text)"
		       :fill (elt colours (mod i (length colours))))))
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

(defun posters-gif-with-big (text color match)
  (interactive "sString: \nsColor: \nsMatching files (regexp): ")
  (meme-gif default-directory
	    match
	    (let ((text-size nil))
	      (lambda (svg width height)
		(unless text-size
		  (setq text-size
			(posters-find-font-size-for-width
			 text width)))
		(svg-text svg (format "%s" text)
			  :font-size (car text-size)
			  :font-weight "bold"
			  :stroke (or color "black")
			  :fill (or color "black")
			  :stroke-width 0
			  :font-family "Futura"
			  :y (+ (/ height 2)
				;; -30
				(/ (cadr text-size) 2))
			  :x (- (1+ (caddr text-size))))))))

(defun posters-find-font-size-for-width (text image-width
					      &optional start end step)
  "Returns a list of FONT-SIZE, HEIGHT, and negative X-OFFSET."
  (setq step (or step 100))
  (cl-loop with prev-height = 10
	   for font-size from (or start 100) upto (or end 1200) by step
	   do (let* ((svg (svg-create (+ image-width 100)
				      (* image-width 2))))
		(svg-text svg (format "%s" text)
			  :font-size font-size
			  :font-weight "bold"
			  :stroke "black"
			  :fill "black"
			  :stroke-width 0
			  :font-family "Futura"
			  :y (/ (* image-width 2) 2)
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
		      (when (> (- (string-to-number (car size)) 2)
			       image-width)
			(cl-return
			 (if (>= step 0.1)
			     (posters-find-font-size-for-width
			      text image-width (- font-size step)
			      (+ font-size step) (/ (float step) 10))
			   (list font-size
				 prev-height
				 (with-temp-buffer
				   (call-process "convert" nil t nil
						 file "-trim" 
						 "-format" "%O" "info:")
				   (goto-char (point-min))
				   (and (looking-at "[+]\\([0-9]+\\)")
					(string-to-number
					 (match-string 1))))))))
		      (setq prev-height (string-to-number (cadr size)))))))))

(defun posters-make-svg-big (file text &optional color)
  (let* ((img (create-image file nil nil :scale 1))
	 (size (image-size img t))
	 (image-height 600)
	 (image-width (truncate (* (/ (* (car size) 1.0) (cdr size))
				   image-height)))
	 (text-size (posters-find-font-size-for-width text image-width))
	 (font-size (car text-size))
	 (svg (svg-create image-width image-height
			  :xmlns:xlink "http://www.w3.org/1999/xlink")))
    ;;(message "text-size %s" text-size)
    (clear-image-cache)
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :width image-width
	       :height image-height)
    (svg-text svg (format "%s" text)
	      :font-size font-size
	      :font-weight "bold"
	      :stroke (or color "black")
	      :fill (or color "black")
	      :stroke-width 0
	      :font-family "Futura"
	      :y (+ (/ image-height 2)
		    ;; -30
		    (/ (cadr text-size) 2))
	      :x (- (1+ (caddr text-size))))
    (when nil
      (svg-text svg (format "%s" "#98")
		:font-size (/ image-height 10)
		:font-weight "bold"
		:stroke "blue"
		:fill "blue"
		:stroke-width 1
		:font-family "Futura"
		:text-anchor "end"
		:y (- image-height 20)
		:x (- image-width 20)))
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

(defun posters-change-image-big (text &optional colorp)
  (interactive "sText: \nP")
  (let ((color nil))
    (when colorp
      (setq color (read-from-minibuffer "Color: ")))
    (clear-image-cache)
    (let ((image (get-text-property (point) 'display)))
      (when (or (not image)
		(not (consp image))
		(not (eq (car image) 'image)))
	(error "No image under point"))
      (let* ((data (cl-getf (cdr image) :data))
	     (file (cl-getf (cdr image) :file))
	     (inhibit-read-only t))
	(when data
	  (with-temp-buffer
	    (set-buffer-multibyte nil)
	    (insert data)
	    (setq file (concat (make-temp-file "poster")
			       "."
			       (car (last (split-string
					   (ewp-content-type data)
					   "/")))))
	    (write-region (point-min) (point-max) file)))
	(let* ((colors '("red" "blue" "green" "orange" "white" "black"
			 "yellow" "pink" "brown" "teal" "purple"
			 "gray" "cyan" "magenta"))
	       (new (posters-make-from-file-big
		     file text
		     (or color
			 (elt colors (random (length colors))))))
	       (edges (window-inside-pixel-edges
		       (get-buffer-window (current-buffer)))))
	  (delete-region (line-beginning-position) (line-end-position))
	  (insert-image
	   (create-image
	    new nil nil
	    :max-width (truncate (* 0.9 (- (nth 2 edges) (nth 0 edges))))
	    :max-height (truncate (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
	   (format "<img src=%S>" new)))))))

(defun posters-find-font-size-for-height (text target-height)
  (cl-loop with prev-width = 10
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
			(cl-return (cons (1- font-size) prev-width)))
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
	 (font-size 350)
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
	(cl-loop for i from 1 upto 16 by 4
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
	(cl-incf y (min 300 (/ 1000 (length texts))))))
    svg))

(defun posters-make-from-file-bistro (file string)
  (let ((svg (posters-make-svg-bistro file string))
	(file (format "/tmp/%s-poster.jpg" (file-name-nondirectory file)))
	(png (format "/tmp/%s-poster.png" (file-name-nondirectory file))))
    (when (file-exists-p file)
      (delete-file file))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (svg-print svg)
      (call-process-region
       (point-min) (point-max)
       "inkscape" nil (get-buffer-create "*convert*") nil
       "-p" (format "--export-filename=%s" png)
       "--export-dpi=96"
       "--export-background=rgb(100%,100%,100%)"
       "--export-background-opacity=1"))
    (call-process "convert" nil nil nil png file)   
    file))

(defun posters-change-image-bistro (text)
  (interactive "sTitle: ")
  (clear-image-cache)
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

;; (find-file (posters-make-from-file-year "~/films/She Done Him Wrong/mpv-shot0001.jpg" "~/tmp/legume.jpg" "XIV"))

(defun posters-make-from-file-year (file food-file week)
  (let ((svg (posters-make-svg-year file food-file
				    (format "MCMXXXIX %s" week)))
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

(defun posters-make-svg-year (file food-file text)
  (let* ((img (create-image file nil nil :scaling 1))
	 (food-size (image-size (create-image food-file nil nil :scaling 1) t))
	 (frame 25)
	 (margin 100)
	 (food-width 1000)
	 (food-height (* (/ (* (cdr food-size) 1.0) (car food-size))
			 (- food-width 0)))
	 (total-width (+ frame frame margin (* food-width 2)))
	 (image-size (image-size img t))
	 (image-height (* (/ (* (cdr image-size) 1.0) (car image-size))
			  food-width))
	 (image-width (* (/ (* (car image-size) 1.0) (cdr image-size))
			 image-height))
	 (font-size 45)
	 (colors '("#345d98" "#d02d1c" "#eed023"
		   "#99b1c9" "#b0dabe" "#3a7359"))
	 (total-height (+ food-height (* frame 2)))
	 (svg (svg-create (+ frame margin (* food-width 1))
			  total-height)))
    (when (> image-height food-height)
      (setq image-width (* (/ (* (car image-size) 1.0) (cdr image-size))
			   food-height))
      (setq image-height (* (/ (* (cdr image-size) 1.0) (car image-size))
			    image-width)))
    (clear-image-cache)
    (svg-rectangle svg 0 0 total-width total-height
		   :fill (seq-random-elt colors))
    (svg-embed svg file (mailcap-file-name-to-mime-type file) nil
	       :x (+ frame (max (/ (- food-width image-width) 2) 0))
	       :y (+ frame (max (/ (- food-height image-height) 2) 0))
	       :width image-width
	       :height image-height)
    (svg-embed svg food-file "image/jpg" nil
	       :x (+ food-width frame margin)
	       :y frame
	       :width food-width
	       :height food-height)
    (svg-text svg (format "%s" text)
	      :font-size font-size
	      :stroke "white"
	      :fill "white"
	      :stroke-width 1
	      :font-family "Futura"
	      :text-anchor "middle"
	      :font-weight "bold"
	      :transform
	      (format "rotate(90) translate(%s %s)"
		      (/ total-height 2)
		      (+ (- food-width) frame
			 (/ font-size -2)
			 -10
			 (/ margin -2))))
    svg))

(defun posters-change-image-year (text)
  (interactive "sTitle: ")
  (clear-image-cache)
  (if (not (looking-at ".*src=\"\\([^\"]+\\)\""))
      (error "Nothing under point")
    (let* ((old (substring-no-properties (match-string 1)))
	   (new (posters-make-from-file-year old old text))
	   (edges (window-inside-pixel-edges
		   (get-buffer-window (current-buffer)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert-image
       (create-image new nil nil
	:max-width (truncate (* 0.9 (- (nth 2 edges) (nth 0 edges))))
	:max-height (truncate (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
       (format "<img src=%S>" new)))))

(defun posters-change-image-director (string color)
  (interactive "sString: \nsColor: ")
  (setq string (upcase string))
  (if (not (looking-at ".*src=\"\\([^\"]+\\)\""))
      (error "Nothing under point")
    (let* ((old (substring-no-properties (match-string 1)))
	   (new (posters-make-from-file-director
		 old string
		 (if (zerop (length color))
		     "white"
		   color)))
	   (edges (window-inside-pixel-edges
		   (get-buffer-window (current-buffer)))))
      (delete-region (line-beginning-position) (line-end-position))
      (insert-image
       (create-image
	new nil nil
	:max-width (truncate (* 0.9 (- (nth 2 edges) (nth 0 edges))))
	:max-height (truncate (* 0.5 (- (nth 3 edges) (nth 1 edges)))))
       (format "<img src=%S>" new)))))

(provide 'posters)

;;; posters.el ends here
