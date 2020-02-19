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
(require 'ht)

(defgroup mc-biome-viewer nil
  "view biomes in a minecraft world from withing Emacs"
  :group 'games)

(defcustom mc-biome-viewer-chunks-in-camera 16
  "No of chunks to show, the value corresponds to the x in an x*x grid."
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

(defvar-local mc-biome-viewer--chunk-cache nil)
(defvar-local mc-biome-viewer--camera-origin-x 0)
(defvar-local mc-biome-viewer--camera-origin-y 0)
(defvar-local mc-biome-viewer--mc-profile nil)
(defvar-local mc-biome-viewer--world-seed nil)
(defvar-local mc-biome-viewer--buffer-start-pos nil)

;; Maps and modes

(defvar mc-biome-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "?"                       #'describe-mode)
    (define-key map "f"                      #'describe-mode)
    (define-key map "b"                       #'describe-mode)))

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
    ))

(defun mc-biome-viewer--init-buffer ()
  "Setup a new buffer for viewing a mc world."
  ;; get initial from server
  (erase-buffer)
  (mc-biome-viewer--init-offsets)
  )

;;;###autoload
(defun mc-biome-viewer-view-seed (seed)
  "Show the Minecraft world with the seed specified by SEED."
  (interactive "sSeed: ")
  (switch-to-buffer "minecraft-biome-view")
  (mc-biome-viewer-mode)
  (mc-biome-viewer--start-server))

;;;###autoload
(defun mc-biome-viewer-view-save (save)
  "Show the local Minecraft world with at the directory specified by SAVE."
  (interactive "seedSave directory name: "))


(provide 'mc-biome-viewer)

;;; mc-biome-viewer.el ends here
