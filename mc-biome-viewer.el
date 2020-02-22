;;; mc-biome-viewer.el --- view biomes in a minecraft world from within Emacs

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

(defcustom mc-biome-viewer-default-profile nil
  "The default minecraft launcher profile to use."
  :group 'mc-biome-viewer
  :type 'string)

(defvar mc-biome-viewer--server-directory
  (concat user-emacs-directory "mc-biome-viewer.jar"))

(defvar mc-biome-viewer--server-port 29171)

(defvar mc-biome-viewer--server-started-p nil)

;; Buffer local internal variables

(defvar-local mc-biome-viewer--chunk-cache (ht-create))
(defvar-local mc-biome-viewer--camera-origin-x 0)
(defvar-local mc-biome-viewer--camera-origin-y 0)
(defvar-local mc-biome-viewer--mc-profile nil)
(defvar-local mc-biome-viewer--world-seed nil)
(defvar-local mc-biome-viewer--x-offset 0)
(defvar-local mc-biome-viewer--y-offset 0)

;; Maps and modes

(defvar mc-biome-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?" 'describe-mode)
    (define-key map "f" 'mc-biome-viewer--forward-x)
    (define-key map "b" 'mc-biome-viewer--backward-x)
    (define-key map "p" 'mc-biome-viewer--forward-y)
    (define-key map "n" 'mc-biome-viewer--backward-y)
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

(defun mc-biome-viewer--draw-buffer (&optional not-found-str)
  "Draw biomes as text.  If a biome is not found insert NOT-FOUND-STR."
  (erase-buffer)
  (mc-biome-viewer--init-offsets)
  (let ((inhibit-read-only t))
    (dotimes (i mc-biome-viewer--y-offset nil)
      (insert "\n"))
    (dotimes (i mc-biome-viewer-row-chunks-in-camera nil)
      (dotimes (j mc-biome-viewer--x-offset nil) (insert " "))
      (dotimes (j mc-biome-viewer-column-chunks-in-camera nil)
	;; Do a dictionary lookup here instead
	(if (ht-contains? mc-biome-viewer--chunk-cache
	     [(+ mc-biome-viewer--camera-origin-x j)
	      (+ mc-biome-viewer--camera-origin-y i)])
	    (insert "Y")
	(insert (if not-found-str (not-found-str) "#"))))
      (insert "\n"))))

(defun mc-biome-viewer--init-buffer ()
  "Setup a new buffer for viewing a mc world."
  (switch-to-buffer "minecraft biome viewer")
  (mc-biome-viewer-mode)
  ;; get initial from server
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-forward-x ()
  (cl-incf mc-biome-viewer--camera-origin-x)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-backward-x ()
  (cl-incf mc-biome-viewer--camera-origin-x)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-forward-y ()
  (cl-incf mc-biome-viewer--camera-origin-y)
  (mc-biome-viewer--draw-buffer))

(defun mc-biome-viewer-backward-y ()
  (cl-incf mc-biome-viewer--camera-origin-y)
  (mc-biome-viewer--draw-buffer))

;;;###autoload
(defun mc-biome-viewer-view-seed (seed)
  "Show the Minecraft world with the seed specified by SEED."
  (interactive "sSeed: ")
  (mc-biome-viewer--init-buffer))

;;;###autoload
(defun mc-biome-viewer-view-save (save)
  "Show the local Minecraft world with at the directory specified by SAVE."
  (interactive "seedSave directory name: "))


(provide 'mc-biome-viewer)

;;; mc-biome-viewer.el ends here
