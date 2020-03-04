;;; mc-biome-viewer.el --- view biomes in a minecraft world from within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: games
;; URL: https://github.com/
;; Package-Requires: ((emacs "26") (cl-lib "0.3") (request "0.3.2") (ht "2.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; mc-biome-viewer shows information about chunks in a minecraft world in an
;; ascii format from within an Emacs buffer.  Worlds can be sourced either from
;; a seed or save directory.

;;; Code:

(require 'request)
(require 'cl-lib)
(require 'ht)

(defgroup mc-biome-viewer nil
  "view biomes in a minecraft world from withing Emacs"
  :group 'games)

(defcustom mc-biome-viewer-row-chunks-in-camera 16
  "How many rows of chunks should be shown in the camera."
  :group 'mc-biome-viewer
  :type 'integer)

(defcustom mc-biome-viewer-column-chunks-in-camera 32
  "How many columns of chunks should be shown in the camera."
  :group 'mc-biome-viewer
  :type 'integer)

(defcustom mc-biome-viewer-default-profile "1.15.1"
  "The default minecraft launcher profile to use."
  :group 'mc-biome-viewer
  :type 'string)

(defcustom mc-biome-viewer-biome-symbol-table
  (let ((table (ht-create)))
    table)
  "A mapping from biome names to characters use to show them on the grid."
  :group 'mc-biome-viewer
  :type 'hash-table)

(defcustom mc-biome-viewer-biome-to-char-map
  #s(hash-table test equal data ("ocean" "o" "plains" "^" "desert" "~" "extreme hills" "△" "forest" "f" "taiga" "‡" "swampland" "%" "river" "=" "hell" "$" "the end" "I" "frozen ocean" "o" "frozen river" "=" "ice plains" "❆" "ice mountains" "▲" "mushroom island" "M" "mushroom island shore" "M" "beach" "." "desert hills" "△" "forest hills" "△" "taiga hills" "△" "extreme hills edge" "△" "jungle" "J" "jungle hills" "△" "jungle edge" "J" "deep ocean" "O" "stone beach" "✧" "cold beach" "." "birch forest" "b" "birch forest hills" "△" "roofed forest" "T" "cold taiga" "‡" "cold taiga hills" "‡" "mega taiga" "⧗" "mega taiga hills" "⧗" "extreme hills+" "△" "savanna" "+" "savanna plateau" "+" "mesa" "#" "mesa plateau f" "#" "mesa plateau" "#" "the end - floating islands" " " "the end - medium island" " " "the end - high island" "▢" "the end - barren island" " " "warm ocean" "o" "lukewarm ocean" "o" "cold ocean" "o" "warm deep ocean" "O" "lukewarm deep ocean" "O" "cold deep ocean" "O" "frozen deep ocean" "O" "the void" " " "sunflower plains" "⁂" "desert m" "~" "extreme hills m" "△" "flower forest" "✿" "taiga m" "‡" "swampland m" "%" "ice plains spikes" "|" "jungle m" " " "jungle edge m" "J" "birch forest m" "b" "birch forest hills m" "b" "roofed forest m" "T" "cold taiga m" "‡" "mega spruce taiga" "‡" "mega spruce taiga (hills)" "‡" "extreme hills+ m" "△" "savanna m" "+" "savanna plateau m" "+" "mesa (bryce)" "#" "mesa plateau f m" "#" "mesa plateau m" "#" "bamboo jungle" "汕" "bamboo jungle hills" "汕"))
    "A mapping from Minecraft biomes to characters used to represent them in the grid."
  :group 'mc-biome-viewer
  :type 'hash-table)

;; Source https://github.com/toolbox4minecraft/amidst/blob/f0229b840b8a9a47d60b558604df45b753b1387e/src/main/java/amidst/mojangapi/world/biome/Biome.java
(defcustom mc-biome-viewer-biome-to-face-map
  #s(hash-table test equal data ("ocean" '(foreground-color . "sky blue") "plains" '(foreground-color . "light green") "desert" '(foreground-color . "yellow") "extreme hills" '(foreground-color . "green") "taiga" '(foreground-color . "dark green") "swampland" '(foreground-color . "brown") "river" '(foreground-color . "sky blue") "hell" '(foreground-color . "red") "the end" nil "frozen ocean" nil "frozen river" nil "ice plains" nil "ice mountains" nil "mushroom island" '(foreground-color . "light pink") "mushroom island shore" '(foreground-color . "light pink") "beach" '(foreground-color . "gold") "desert hills" '(foreground-color . "yellow") "forest hills" '(foreground-color . "green") "taiga hills" '(foreground-color . "dark green") "extreme hills edge" '(foreground-color . "chocolate") "jungle" '(foreground-color . "spring green") "jungle hills" '(foreground-color . "spring green") "jungle edge" '(foreground-color . "spring green") "deep ocean" '(foreground-color . "dark blue") "stone beach" '(foreground-color . "grey") "cold beach" nil "birch forest" '(foreground-color . "lawn green") "birch forest hills" '(foreground-color . "lawn green") "roofed forest" '(foreground-color . "green") "cold taiga" nil "cold taiga hills" nil "mega taiga" '(foreground-color . "olive") "mega taiga hills" '(foreground-color . "olive") "extreme hills+" '(foreground-color . "chocolate") "savanna" '(foreground-color . "orange") "savanna plateau" '(foreground-color . "orange") "mesa" '(foreground-color . "red") "mesa plateau f" '(foreground-color . "red") "mesa plateau" '(foreground-color . "red") "the end - floating islands" nil "the end - medium island" nil "the end - high island" nil "the end - barren island" nil "warm ocean" '(foreground-color . "cyan") "lukewarm ocean" '(foreground-color . "cyan") "cold ocean" nil "warm deep ocean" '(foreground-color . "cyan") "lukewarm deep ocean" '(foreground-color . "cyan") "cold deep ocean" nil "frozen deep ocean" nil "the void" nil "sunflower plains" '(foreground-color . "light green") "desert m" '(foreground-color . "yellow") "extreme hills m" '(foreground-color . "yellow") "flower forest" '(foreground-color . "light green") "taiga m" '(foreground-color . "dark green") "swampland m" '(foreground-color . "brown") "ice plains spikes" '(foreground-color . "violet") "jungle m" '(foreground-color . "spring green") "jungle edge m" '(foreground-color . "spring green") "birch forest m" '(foreground-color . "lawn green") "birch forest hills m" '(foreground-color . "lawn green") "roofed forest m" '(foreground-color . "green") "cold taiga m" nil "mega spruce taiga" nil "mega spruce taiga (hills)" nil "extreme hills+ m" '(foreground-color . "chocolate") "savanna m" '(foreground-color . "orange") "savanna plateau m" '(foreground-color . "orange") "mesa (bryce)" '(foreground-color . "red") "mesa plateau f m" '(foreground-color . "red") "mesa plateau m" '(foreground-color . "red") "bamboo jungle" '(foreground-color . "spring green") "bamboo jungle hills" '(foreground-color . "spring green")))
  "A hash table mapping biome names to faces to be applied to them on the grid. Specifically, values in this hash can be any valid value for the face property described in https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html#Overlay-Properties"
  :group 'mc-biome-viewer
  :type 'hash-table)

(defvar mc-biome-viewer--server-directory
  (concat user-emacs-directory "mc-biome-viewer.jar"))

(defvar mc-biome-viewer--server-port 29171)

(defvar mc-biome-viewer--server-started-p nil)

;; Buffer local internal variables

(defvar-local mc-biome-viewer--chunk-cache nil)
(defvar-local mc-biome-viewer--camera-origin-x 0)
(defvar-local mc-biome-viewer--camera-origin-y 0)
(defvar-local mc-biome-viewer--mc-profile nil)
(defvar-local mc-biome-viewer--world-seed nil)
(defvar-local mc-biome-viewer--x-offset 0)
(defvar-local mc-biome-viewer--y-offset 0)
(defvar-local mc-biome-viewer--seed nil)
(defvar-local mc-biome-viewer--save nil)

;; Maps and modes

(defvar mc-biome-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?" #'describe-mode)
    (define-key map "f" #'mc-biome-viewer-forward-x)
    (define-key map "b" #'mc-biome-viewer-backward-x)
    (define-key map "p" #'mc-biome-viewer-forward-y)
    (define-key map "n" #'mc-biome-viewer-backward-y)
    map))

(define-derived-mode mc-biome-viewer-mode special-mode "mc-biome-viewer"
  "A mode for showing minecraft worlds."
  (setq buffer-read-only t
        truncate-lines   t)
  (buffer-disable-undo))

(defun mc-biome-viewer--start-server ()
  "Start a mc-biome-viewer server unless one has already started."
  (unless mc-biome-viewer--server-started
    (start-process "mc-biome-viewer-server" nil
		   (concat "java -jar " mc-biome-viewer--server-directory))))

(defun mc-biome-viewer--init-offsets ()
  "Initialize the offsets for the current mc-biome-viewer buffer."
  (let ((width (window-text-width))
	(height (window-text-height)))
    (setq mc-biome-viewer--x-offset
	  (max 0 (- (/ width 2) (/ mc-biome-viewer-column-chunks-in-camera 2))))
    (setq mc-biome-viewer--y-offset
	  (max 0 (- (/ height 2) (/ mc-biome-viewer-row-chunks-in-camera 2))))))

(defun mc-biome-viewer--draw-biome (biome-str unknown-str)
  "Draw the biome specified by BIOME-STR at the current cursor position, else draw UNKNOWN-STR."
  (if (ht-contains? mc-biome-viewer-biome-to-char-map biome-str)
      (insert (ht-get mc-biome-viewer-biome-to-char-map biome-str))
    (insert unknown-str))
  ;; Add overlay
  (if (ht-contains? mc-biome-viewer-biome-to-face-map biome-str)
      (let ((overlay (make-overlay (1- (point)) (point))))
	(overlay-put overlay 'face
		     (ht-get mc-biome-viewer-biome-to-face-map biome-str)))))

(defun mc-biome-viewer--draw-buffer (&optional not-found-str)
  "Draw biomes as text in the current buffer.  If a biome is not found insert NOT-FOUND-STR."
  (mc-biome-viewer--init-offsets)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (i mc-biome-viewer--y-offset nil)
      (insert "\n"))
    (save-excursion
      (dotimes (i mc-biome-viewer-row-chunks-in-camera nil)
	(dotimes (j mc-biome-viewer--x-offset nil) (insert " "))
	(dotimes (j mc-biome-viewer-column-chunks-in-camera nil)
	  (let* ((true-y (- mc-biome-viewer-row-chunks-in-camera i))
		 (vec (vector (+ mc-biome-viewer--camera-origin-x j)
			      (+ mc-biome-viewer--camera-origin-y true-y))))
	    (if (ht-contains? mc-biome-viewer--chunk-cache vec)
		(let ((biome-str (ht-get mc-biome-viewer--chunk-cache vec)))
		  (mc-biome-viewer--draw-biome biome-str "?"))
	      (insert (if not-found-str not-found-str "#")))))
	(insert "\n")))
    (forward-char mc-biome-viewer--x-offset)))

(defun mc-biome-viewer--init-buffer ()
  "Setup a new buffer for viewing a mc world."
  (switch-to-buffer (generate-new-buffer-name "Minecraft Biome Viewer"))
  (mc-biome-viewer-mode)
  (setq mc-biome-viewer--chunk-cache (ht-create))
  ;; get initial from server
  (mc-biome-viewer--draw-buffer))

(cl-defun mc-biome-viewer--request-biomes-seed
    (seed chunk-start-x chunk-start-y chunk-end-x chunk-end-y
	  &key (profile mc-biome-viewer-default-profile) callback)
  "Request chunks from the Minecraft world generated by SEED (using the specified PROFILE) in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  Call CALLBACK with the xml data."
  (request
   (concat "http://localhost:"
	   (number-to-string mc-biome-viewer--server-port)
	   "/biome/seed")
   :params `(("seed" . ,seed)
	     ("chunkStartX" . ,chunk-start-x) ("chunkEndX" . ,chunk-end-x)
	     ("chunkStartY" . ,chunk-start-y) ("chunkEndY" . ,chunk-end-y)
	     ("profile" . ,(if profile profile mc-biome-viewer-default-profile)))
   :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (when callback (funcall callback data))))))

(cl-defun mc-biome-viewer--request-biomes-save
    (save chunk-start-x chunk-start-y chunk-end-x chunk-end-y
	  &key (profile mc-biome-viewer-default-profile) callback)
  "Request chunks from the Minecraft world on local storage specified by SAVE (using the specified PROFILE) in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  Call CALLBACK with the xml data."
  (request
   (concat "http://localhost:"
	   (number-to-string mc-biome-viewer--server-port)
	   "/biome/seed")
   :params `(("save" . ,save)
	     ("chunkStartX" . ,chunk-start-x) ("chunkEndX" . ,chunk-end-x)
	     ("chunkStartY" . ,chunk-start-y) ("chunkEndY" . ,chunk-end-y)
	     ("profile" . ,(if profile profile mc-biome-viewer-default-profile)))
   :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (when callback (funcall callback data))))))

(cl-defun mc-biome-viewer--update-biomes (chunk-start-x chunk-start-y chunk-end-x chunk-end-y &key (profile mc-biome-viewer-default-profile) callback)
  "Request chunks from the Minecraft world shown in the current buffer in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  Call CALLBACK with the xml data."
  (if mc-biome-viewer--seed
      (mc-biome-viewer--request-biomes-seed mc-biome-viewer--seed
					    chunk-start-x chunk-start-y
					    chunk-end-x chunk-end-y
					    profile callback)
    (mc-biome-viewer--request-biomes-save mc-biome-viewer--save
					  chunk-start-x chunk-start-y
					  chunk-end-x chunk-end-y
					  profile callback)))

(defun mc-biome-viewer--update-from-xml (data)
  "Update the chunk cache from XML DATA retrieved from a mc biome viewer server."
  (cl-loop for e in (cddr data) do
	   (let ((x (car (last (caddr e))))
		 (y (car (last (cadddr e))))
		 (biome (downcase (car (last (car (last e)))))))
	     (ht-set mc-biome-viewer--chunk-cache
		     (vector (string-to-number x) (string-to-number y)) biome)))
  (mc-biome-viewer--draw-buffer "/"))

(defun mc-biome-viewer-forward-x ()
  (interactive)
  (cl-incf mc-biome-viewer--camera-origin-x)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-backward-x ()
  (interactive)
  (cl-decf mc-biome-viewer--camera-origin-x)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-forward-y ()
  (interactive)
  (cl-incf mc-biome-viewer--camera-origin-y)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-backward-y ()
  (interactive)
  (cl-decf mc-biome-viewer--camera-origin-y)
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-view-seed (seed)
  "Show the Minecraft world with the seed specified by SEED."
  (interactive "sSeed: ")
  (mc-biome-viewer--init-buffer)
  (setq mc-biome-viewer--seed seed)
  (message "Contacting server...")
  (mc-biome-viewer--request-biomes-seed seed 0 0
					mc-biome-viewer-column-chunks-in-camera
					mc-biome-viewer-row-chunks-in-camera
					:callback #'mc-biome-viewer--update-from-xml))

;;;###autoload
(defun mc-biome-viewer-view-save (save)
  "Show the local Minecraft world with at the directory specified by SAVE."
  (interactive "seedSave directory name: ")
  (mc-biome-viewer--init-buffer
   (setq mc-biome-viewer--save save)
   (message "contacting server...")))


(provide 'mc-biome-viewer)

;;; mc-biome-viewer.el ends here
