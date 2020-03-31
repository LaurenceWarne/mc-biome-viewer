;;; mc-biome-viewer.el --- View biomes in a Minecraft world -*- lexical-binding: t -*-

;; Copyright (C) 2020 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: games
;; URL: https://github.com/
;; Package-Requires: ((emacs "26") (dash "2.17.0") (cl-lib "0.3") (request "0.3.2") (ht "2.2") (f "0.2.0"))

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

(require 'cl-lib)
(require 'url)
(require 'dash)
(require 'request)
(require 'f)
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

(defcustom mc-biome-viewer-default-version "1.15.1"
  "The default minecraft launcher profile to use."
  :group 'mc-biome-viewer
  :type 'string)

(defcustom mc-biome-viewer-biome-symbol-table
  (let ((table (ht-create)))
    table)
  "A mapping from biome names to characters use to show them on the grid."
  :group 'mc-biome-viewer
  :type 'hash-table)

(defcustom mc-biome-viewer-show-label t
  "If non-nil draw a label below the mc-biome-viewer grid showing information about the biome at the cursor."
  :group 'mc-biome-viewer
  :type 'boolean)

(defcustom mc-biome-viewer-colour-biomes t
  "If non-nil colour biomes in the mc-biome-viewer grid according to the value of the biome in MC-BIOME-VIEWER-BIOME-TO-FACE-MAP."
  :group 'mc-biome-viewer
  :type 'boolean)

(defcustom mc-biome-viewer-biome-to-char-map
  '#s(hash-table test equal data ("bamboo jungle hills" ?Y
                                  "bamboo jungle" ?Y
                                  "beach" ?.
                                  "birch forest hills m" ?b
                                  "birch forest hills" ?△
                                  "birch forest m" ?b
                                  "birch forest" ?b
                                  "cold beach" ?.
                                  "cold deep ocean" ?O
                                  "cold ocean" ?o
                                  "cold taiga hills" ?‡
                                  "cold taiga m" ?‡
                                  "cold taiga" ?‡
                                  "deep ocean" ?O
                                  "desert hills" ?△
                                  "desert m" ?~
                                  "desert" ?~
                                  "extreme hills edge" ?△
                                  "extreme hills m" ?△
                                  "extreme hills" ?△
                                  "extreme hills+ m" ?△
                                  "extreme hills+" ?△
                                  "flower forest" ?✿
                                  "forest hills" ?△
                                  "forest" ?f
                                  "frozen deep ocean" ?O
                                  "frozen ocean" ?o
                                  "frozen river" ?=
                                  "hell" ?$
                                  "ice mountains" ?▲
                                  "ice plains spikes" ?|
                                  "ice plains" ?❆
                                  "jungle edge m" ?J
                                  "jungle edge" ?J
                                  "jungle hills" ?△
                                  "jungle m" ?J
                                  "jungle" ?J
                                  "lukewarm deep ocean" ?O
                                  "lukewarm ocean" ?o
                                  "mega spruce taiga (hills)" ?‡
                                  "mega spruce taiga" ?‡
                                  "mega taiga hills" ?&
                                  "mega taiga" ?&
                                  "mesa (bryce)" ?#
                                  "mesa plateau f m" ?#
                                  "mesa plateau f" ?#
                                  "mesa plateau m" ?#
                                  "mesa plateau" ?#
                                  "mesa" ?#
                                  "mushroom island shore" ?M
                                  "mushroom island" ?M
                                  "ocean" ?o
                                  "plains" ?^
                                  "river" ?=
                                  "roofed forest m" ?T
                                  "roofed forest" ?T
                                  "savanna m" ?+
                                  "savanna plateau m" ?+
                                  "savanna plateau" ?+
                                  "savanna" ?+
                                  "stone beach" ?✧
                                  "sunflower plains" ?⁂
                                  "swampland m" ?%
                                  "swampland" ?%
                                  "taiga hills" ?△
                                  "taiga m" ?‡
                                  "taiga" ?‡
                                  "the end - barren island" ?\s
                                  "the end - floating islands" ?\s
                                  "the end - high island" ?▢
                                  "the end - medium island" ?\s
                                  "the end" ?I
                                  "the void" ?\s
                                  "warm deep ocean" ?O
                                  "warm ocean" ?o))
    "A mapping from Minecraft biomes to characters used to represent them in the grid."
  :group 'mc-biome-viewer
  :type 'hash-table)

;; Source https://github.com/toolbox4minecraft/amidst/blob/f0229b840b8a9a47d60b558604df45b753b1387e/src/main/java/amidst/mojangapi/world/biome/Biome.java
(defcustom mc-biome-viewer-biome-to-face-map
  #s(hash-table test equal data ("bamboo jungle hills" (:foreground "spring green")
                                 "bamboo jungle" (:foreground "spring green")
                                 "beach" (:foreground "gold")
                                 "birch forest hills m" (:foreground "lawn green")
                                 "birch forest hills" (:foreground "lawn green")
                                 "birch forest m" (:foreground "lawn green")
                                 "birch forest" (:foreground "lawn green")
                                 "cold beach" (:foreground "white")
                                 "cold deep ocean" (:foreground "white")
                                 "cold ocean" (:foreground "white")
                                 "cold taiga hills" (:foreground "white")
                                 "cold taiga m" (:foreground "white")
                                 "cold taiga" (:foreground "white")
                                 "deep ocean" (:foreground "dark blue")
                                 "desert hills" (:foreground "yellow")
                                 "desert m" (:foreground "yellow")
                                 "desert" (:foreground "yellow")
                                 "extreme hills edge" (:foreground "chocolate")
                                 "extreme hills m" (:foreground "yellow")
                                 "extreme hills" (:foreground "green")
                                 "extreme hills+ m" (:foreground "chocolate")
                                 "extreme hills+" (:foreground "chocolate")
                                 "flower forest" (:foreground "light green")
                                 "forest hills" (:foreground "green")
                                 "forest" (:foreground "green")
                                 "frozen deep ocean" (:foreground "white")
                                 "frozen ocean" (:foreground "white")
                                 "frozen river" (:foreground "white")
                                 "hell" (:foreground "red")
                                 "ice mountains" (:foreground "white")
                                 "ice plains spikes" (:foreground "violet")
                                 "ice plains" (:foreground "white")
                                 "jungle edge m" (:foreground "spring green")
                                 "jungle edge" (:foreground "spring green")
                                 "jungle hills" (:foreground "spring green")
                                 "jungle m" (:foreground "spring green")
                                 "jungle" (:foreground "spring green")
                                 "lukewarm deep ocean" (:foreground "cyan")
                                 "lukewarm ocean" (:foreground "cyan")
                                 "mega spruce taiga (hills)" (:foreground "white")
                                 "mega spruce taiga" (:foreground "white")
                                 "mega taiga hills" (:foreground "olive")
                                 "mega taiga" (:foreground "olive")
                                 "mesa (bryce)" (:foreground "red")
                                 "mesa plateau f m" (:foreground "red")
                                 "mesa plateau f" (:foreground "red")
                                 "mesa plateau m" (:foreground "red")
                                 "mesa plateau" (:foreground "red")
                                 "mesa" (:foreground "red")
                                 "mushroom island shore" (:foreground "light pink")
                                 "mushroom island" (:foreground "light pink")
                                 "ocean" (:foreground "sky blue")
                                 "plains" (:foreground "light green")
                                 "river" (:foreground "sky blue")
                                 "roofed forest m" (:foreground "green")
                                 "roofed forest" (:foreground "green")
                                 "savanna m" (:foreground "orange")
                                 "savanna plateau m" (:foreground "orange")
                                 "savanna plateau" (:foreground "orange")
                                 "savanna" (:foreground "orange")
                                 "stone beach" (:foreground "grey")
                                 "sunflower plains" (:foreground "light green")
                                 "swampland m" (:foreground "brown")
                                 "swampland" (:foreground "brown")
                                 "taiga hills" (:foreground "dark green")
                                 "taiga m" (:foreground "dark green")
                                 "taiga" (:foreground "dark green")
                                 "the end - barren island" (:foreground "white")
                                 "the end - floating islands" (:foreground "white")
                                 "the end - high island" (:foreground "white")
                                 "the end - medium island" (:foreground "white")
                                 "the end" (:foreground "white")
                                 "the void" (:foreground "white")
                                 "warm deep ocean" (:foreground "cyan")
                                 "warm ocean" (:foreground "cyan")))
   "A hash table mapping biome names to faces to be applied to them on the grid. Specifically, values in this hash can be any valid value for the face property described in https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlay-Properties.html#Overlay-Properties"
  :group 'mc-biome-viewer
  :type 'hash-table)

(defvar mc-biome-viewer--server-directory
  (concat user-emacs-directory "mc-biome-viewer"))

(defvar mc-biome-viewer--server-url "https://github.com/LaurenceWarne/mc-biome-map-server/releases/download/")

(defvar mc-biome-viewer--server-version "v0.1")

(defvar mc-biome-viewer--jar-name "mc-biome-map-server-all")

(defvar mc-biome-viewer--server-port 29171)

(defvar mc-biome-viewer--server-started nil)

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
(defvar-local mc-biome-viewer--chunk-size 16)

;; Maps and modes

(defvar mc-biome-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?" #'describe-mode)
    (define-key map "f" #'mc-biome-viewer-forward-x)
    (define-key map "b" #'mc-biome-viewer-backward-x)
    (define-key map "p" #'mc-biome-viewer-forward-y)
    (define-key map "n" #'mc-biome-viewer-backward-y)
    (define-key map "j" #'mc-biome-viewer-centre-camera)
    (define-key map [C-right] #'mc-biome-viewer-forward-x)
    (define-key map [C-left] #'mc-biome-viewer-backward-x)
    (define-key map [C-up] #'mc-biome-viewer-forward-y)
    (define-key map [C-down]  #'mc-biome-viewer-backward-y)
    map))

(define-derived-mode mc-biome-viewer-mode special-mode "mc-biome-viewer"
  "A mode for showing Minecraft worlds."
  (setq buffer-read-only t
        truncate-lines   t)
  (set (make-local-variable 'post-command-hook) nil)
  (set (make-local-variable 'pre-command-hook) nil)
  (add-hook 'post-command-hook
	    (lambda () (when mc-biome-viewer-show-label (mc-biome-viewer--draw-label :delete t))) nil t)
  (buffer-disable-undo))

;; Internal functions

(defun mc-biome-viewer--get-full-jar-name ()
  "Get the full name of the server file, including the version and file extension."
  (concat mc-biome-viewer--jar-name "-"
	  mc-biome-viewer--server-version ".jar"))

(defun mc-biome-viewer--get-true-url (gh-url)
  "Get the raw url from the GH release file at the url GH-URL."
  (let ((response (request gh-url :sync t)))
    (request-response-url response)))

(defun mc-biome-viewer--download-server ()
  "Download the mc biome viewer server jar file, replacing it if it alread exists."
  (url-copy-file (mc-biome-viewer--get-true-url
		  (concat mc-biome-viewer--server-url
			  mc-biome-viewer--server-version "/"
			  "mc-biome-map-server-all.jar"))
		 (concat mc-biome-viewer--server-directory "/"
			 (mc-biome-viewer--get-full-jar-name)) t))

(defun mc-biome-viewer--start-server ()
  "Start a mc-biome-viewer server unless one has already started."
  (unless mc-biome-viewer--server-started
    (let ((server-location (concat mc-biome-viewer--server-directory "/"
				  (mc-biome-viewer--get-full-jar-name))))
      (when (not (file-exists-p server-location))
	(f-delete mc-biome-viewer--server-directory t)  ; Remove old versions if needed
	(f-mkdir mc-biome-viewer--server-directory)
	(mc-biome-viewer--download-server))
      (start-process-shell-command "mc-biome-viewer-server" nil
		     (concat "java -jar " server-location))
      (setq mc-biome-viewer--server-started t))))

(defun mc-biome-viewer--get-biome-coord-at-cursor ()
  "Return the biome coordinate at the cursor, or nil if the cursor does not lie on the grid."
  (let* ((row (line-number-at-pos))
	 (column (current-column))
	 (camera-row (- row mc-biome-viewer--y-offset))
	 (camera-y (- mc-biome-viewer-row-chunks-in-camera camera-row))
	 (camera-col (- column mc-biome-viewer--x-offset)))
    (if (and (>= camera-y 0) (>= camera-col 0)
	     (< camera-y mc-biome-viewer-row-chunks-in-camera)
	     (< camera-col mc-biome-viewer-column-chunks-in-camera))
	(vector (+ camera-col mc-biome-viewer--camera-origin-x)
		(+ camera-y mc-biome-viewer--camera-origin-y))
      nil)))

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
  (when (and mc-biome-viewer-colour-biomes
	     (ht-contains? mc-biome-viewer-biome-to-face-map biome-str))
      (let ((overlay (make-overlay (1- (point)) (point))))
	(overlay-put overlay 'face
		     (ht-get mc-biome-viewer-biome-to-face-map biome-str)))))

(defun mc-biome-viewer--create-line-overlay (start end face)
  "Create an overlay from START to END of FACE."
  (let ((overlay (make-overlay start end)))
	  (overlay-put overlay 'face face)))

(cl-defun mc-biome-viewer--draw-row (start-x y &key (not-found-char ??))
  "Draw a row at the current cursor position starting with the biome at the biome coordinate (START-X, Y)."
  (let ((first-biome (ht-get mc-biome-viewer--chunk-cache (vector start-x y)
			     not-found-char)))
    (cl-do ((i (1+ start-x) (1+ i))
	    (last-biome first-biome current-biome)
	    (current-biome first-biome (ht-get mc-biome-viewer--chunk-cache
					       (vector i y) not-found-char))
	    (biome-run 0 (1+ biome-run)))
	((> i (+ start-x mc-biome-viewer-column-chunks-in-camera)) nil)
      (when (not (equal current-biome last-biome))
	(insert-char (ht-get mc-biome-viewer-biome-to-char-map last-biome not-found-char)
		     biome-run)
	(when mc-biome-viewer-colour-biomes
	  (mc-biome-viewer--create-line-overlay
	   (- (point) biome-run) (point)
	   (ht-get mc-biome-viewer-biome-to-face-map last-biome nil)))
	(setq biome-run 0))
      (when (= i (+ start-x mc-biome-viewer-column-chunks-in-camera))
	(insert-char (ht-get mc-biome-viewer-biome-to-char-map
			     current-biome not-found-char)
		     biome-run)
	(when mc-biome-viewer-colour-biomes
	  (mc-biome-viewer--create-line-overlay
	   (- (point) biome-run) (point)
	   (ht-get mc-biome-viewer-biome-to-face-map current-biome nil)))))))

(defun mc-biome-viewer--draw-buffer (&optional not-found-char)
  "Draw biomes as text in the current buffer.  If a biome is not found insert NOT-FOUND-CHAR."
  (mc-biome-viewer--init-offsets)
  (let ((inhibit-read-only t)
	(prev-point (point))
	(prev-biome (mc-biome-viewer--get-biome-coord-at-cursor)))
    (erase-buffer)
    (remove-overlays)  ; Else causes huge lag
    (insert-char ?\n mc-biome-viewer--y-offset)
    (save-excursion
      (dotimes (i mc-biome-viewer-row-chunks-in-camera nil)
	(insert-char ?\s mc-biome-viewer--x-offset)
	(let* ((true-y (1- (+ mc-biome-viewer--camera-origin-y
			      (- mc-biome-viewer-row-chunks-in-camera i)))))
	    (mc-biome-viewer--draw-row mc-biome-viewer--camera-origin-x true-y))
	(insert-char ?\n)))
    (if (> prev-point 1) (goto-char prev-point) (forward-char mc-biome-viewer--x-offset))
    (when mc-biome-viewer-show-label
      (save-excursion (goto-char (point-max)) (insert-char ?\n)
		      (mc-biome-viewer--draw-label :position prev-biome)))))

(defun mc-biome-viewer--init-buffer ()
  "Setup a new buffer for viewing a mc world."
  (switch-to-buffer (generate-new-buffer-name "Minecraft Biome Viewer"))
  (mc-biome-viewer-mode)
  (setq mc-biome-viewer--chunk-cache (ht-create))
  ;; get initial from server
  (mc-biome-viewer--draw-buffer))

(cl-defun mc-biome-viewer--draw-label (&key delete (position (mc-biome-viewer--get-biome-coord-at-cursor)))
  "Draw information about the biome at the cursor or a specified point."
  (save-excursion
    (let* ((biome (ht-get mc-biome-viewer--chunk-cache position))
	   (inhibit-read-only t)
	   (region-start (progn (goto-char (point-max)) (forward-line -1)
				(line-beginning-position)))
	   (region-end (progn (forward-line) (line-end-position))))
      (remove-overlays region-start region-end)
      (when delete (delete-region region-start region-end))
      (insert "Biome: " (if biome biome "?") "\n")
      (when biome
	(let ((overlay (make-overlay (- (point) (1+ (length biome))) (point))))
	  (overlay-put overlay 'face (ht-get mc-biome-viewer-biome-to-face-map
					     biome))))
      (insert "(x z): " (if position (format "%s" (--map (* mc-biome-viewer--chunk-size it) position)) "N/A")))))

(cl-defun mc-biome-viewer--request-biomes-seed
    (seed chunk-start-x chunk-start-y chunk-end-x chunk-end-y
	  &key (profile mc-biome-viewer-default-version)
	  (error-checker #'mc-biome-viewer--error-check-xml)
	  callback)
  "Request chunks from the Minecraft world generated by SEED (using the specified PROFILE) in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  If an error is detected (using ERROR CHECKER) with the returned data message this error, else call CALLBACK with the xml data."
  (request
   (concat "http://localhost:"
	   (number-to-string mc-biome-viewer--server-port)
	   "/biome/seed")
   :params `(("seed" . ,seed)
	     ("chunkStartX" . ,chunk-start-x) ("chunkEndX" . ,chunk-end-x)
	     ("chunkStartY" . ,chunk-start-y) ("chunkEndY" . ,chunk-end-y)
	     ("profile" . ,(if profile profile mc-biome-viewer-default-version)))
   :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (let ((error-string (funcall error-checker data)))
		 (if error-string (message error-string)
		   (when callback (funcall callback data))))))))

(cl-defun mc-biome-viewer--request-biomes-save
    (save chunk-start-x chunk-start-y chunk-end-x chunk-end-y
	  &key (profile mc-biome-viewer-default-version)
	  (error-checker #'mc-biome-viewer--error-check-xml)
	  callback)
  "Request chunks from the Minecraft world on local storage specified by SAVE (using the specified PROFILE) in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  If an error is detected (using ERROR CHECKER) with the returned data message this error, else call CALLBACK with the xml data."
  (request
   (concat "http://localhost:"
	   (number-to-string mc-biome-viewer--server-port)
	   "/biome/save")
   :params `(("save" . ,save)
	     ("chunkStartX" . ,chunk-start-x) ("chunkEndX" . ,chunk-end-x)
	     ("chunkStartY" . ,chunk-start-y) ("chunkEndY" . ,chunk-end-y)
	     ("profile" . ,(if profile profile mc-biome-viewer-default-version)))
   :parser (lambda () (libxml-parse-xml-region (point) (point-max)))
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
	       (let ((error-string (funcall error-checker data)))
		 (if error-string (message error-string)
		   (when callback (funcall callback data))))))))

(cl-defun mc-biome-viewer--update-biomes
    (&key (chunk-start-x mc-biome-viewer--camera-origin-x)
	  (chunk-start-y mc-biome-viewer--camera-origin-y)
	  (chunk-end-x (+ mc-biome-viewer--camera-origin-x
			    mc-biome-viewer-column-chunks-in-camera))
	  (chunk-end-y (+ mc-biome-viewer--camera-origin-y
			    mc-biome-viewer-row-chunks-in-camera))
	  (profile mc-biome-viewer-default-version)
	  (error-checker #'mc-biome-viewer--error-check-xml)
	  callback)
  "Request chunks from the Minecraft world shown in the current buffer in the integral square described by CHUNK-START-X, CHUNK-START-Y, CHUNK-END-X and CHUNK-END-Y (representing chunk coordinates) from a mc-biome-server.  If an error is detected (using ERROR CHECKER) with the returned data message this error, else call CALLBACK with the xml data."
  (if mc-biome-viewer--seed
      (mc-biome-viewer--request-biomes-seed mc-biome-viewer--seed
					    chunk-start-x chunk-start-y
					    chunk-end-x chunk-end-y
					    :error-checker error-checker
					    :profile profile :callback callback)
    (mc-biome-viewer--request-biomes-save mc-biome-viewer--save
					  chunk-start-x chunk-start-y
					  chunk-end-x chunk-end-y
					  :error-checker error-checker
					  :profile profile :callback callback)))

(defun mc-biome-viewer--error-check-xml (data)
  "If the passed xml data DATA is an error message, return the message, else return nil."
  (if (eq 'error (caaddr data))
      (car (last (caddr data)))
    nil))

(defun mc-biome-viewer--update-from-xml (data)
  "Update the chunk cache from XML DATA retrieved from a mc biome viewer server."
  (cl-loop for e in (cddr data) do
	   (let ((x (car (last (caddr e))))
		 (y (car (last (cadddr e))))
		 (biome (downcase (car (last (car (last e)))))))
	     (ht-set mc-biome-viewer--chunk-cache
		     (vector (string-to-number x) (string-to-number y)) biome)))
  (mc-biome-viewer--draw-buffer "?")
  )

(cl-defun mc-biome-viewer--continual-call (fn &key (delay 0.5) (times 5))
  (cl-loop repeat times do
    (sleep-for delay)
    (funcall fn)))

;; Interactive functions

;;;###autoload
(defun mc-biome-viewer-forward-x ()
  "Move the camera one chunk to the left."
  (interactive)
  (cl-incf mc-biome-viewer--camera-origin-x)
  (let ((start (+ mc-biome-viewer--camera-origin-x
		  mc-biome-viewer-column-chunks-in-camera)))
    (mc-biome-viewer--update-biomes :chunk-start-x start :chunk-end-x (+ 2 start)
				    :callback #'mc-biome-viewer--update-from-xml))
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-backward-x ()
  "Move the camera one chunk to the right."
  (interactive)
  (cl-decf mc-biome-viewer--camera-origin-x)
  (let ((start mc-biome-viewer--camera-origin-x))
    (mc-biome-viewer--update-biomes :chunk-start-x (- start 2) :chunk-end-x start
				    :callback #'mc-biome-viewer--update-from-xml))
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-forward-y ()
  "Move the camera one chunk upwards."
  (interactive)
  (cl-incf mc-biome-viewer--camera-origin-y)
  (let ((start (+ mc-biome-viewer--camera-origin-y
		  mc-biome-viewer-row-chunks-in-camera)))
    (mc-biome-viewer--update-biomes :chunk-start-y start :chunk-end-y (+ 2 start)
				    :callback #'mc-biome-viewer--update-from-xml))
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-backward-y ()
  "Move the camera one chunk downwards."
  (interactive)
  (cl-decf mc-biome-viewer--camera-origin-y)
  (let ((start mc-biome-viewer--camera-origin-y))
    (mc-biome-viewer--update-biomes :chunk-start-y (- start 2) :chunk-end-y start
				    :callback #'mc-biome-viewer--update-from-xml))
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-centre-camera (x z)
  "Centre the camera at the specified world (X, Z) coordinate."
  (interactive "nX: \nnZ: ")
  (setq mc-biome-viewer--camera-origin-x
	(- (/ x mc-biome-viewer--chunk-size) (/ mc-biome-viewer-column-chunks-in-camera 2)))
  (setq mc-biome-viewer--camera-origin-y
	(- (/ z mc-biome-viewer--chunk-size) (/ mc-biome-viewer-row-chunks-in-camera 2)))
  (mc-biome-viewer--update-biomes :callback #'mc-biome-viewer--update-from-xml))

;;;###autoload
(defun mc-biome-viewer-view-seed (seed)
  "Show the Minecraft world with the seed specified by SEED.  The version used by the profile mc-biome-viewer-default-version will be used to generate the world from the seed."
  (interactive "sSeed: ")
  (message "Starting server...")
  (mc-biome-viewer--start-server)
  (mc-biome-viewer--init-buffer)
  (setq mc-biome-viewer--seed seed)
  (message "Contacting server...")
  (make-thread (lambda () (mc-biome-viewer--continual-call
      (lambda () (mc-biome-viewer--update-biomes
		  :callback #'mc-biome-viewer--update-from-xml))))))

;;;###autoload
(defun mc-biome-viewer-view-save (save)
  "Show the local Minecraft world at the directory specified by SAVE.  An example directory would be ~/.minecraft/my-profile/saves/my-world."
  (interactive "DSave directory: ")
  (message "Starting server...")
  (mc-biome-viewer--start-server)
  (mc-biome-viewer--init-buffer)
  (setq mc-biome-viewer--save (expand-file-name save))
  (message "contacting server...")
  (make-thread (lambda () (mc-biome-viewer--continual-call
      (lambda () (mc-biome-viewer--update-biomes
		  :callback #'mc-biome-viewer--update-from-xml))))))


(provide 'mc-biome-viewer)

;;; mc-biome-viewer.el ends here
