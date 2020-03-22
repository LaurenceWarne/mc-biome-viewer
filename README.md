# Emacs Biome Viewer

A tool for viewing your Minecraft worlds from within Emacs.

![mc-biome-viewer example](https://user-images.githubusercontent.com/17688577/76894381-f21ac080-6885-11ea-8268-0d4f4cd329df.png)

## Keys

| Key | Command                             | Description                                        |
|-----|-------------------------------------|----------------------------------------------------|
| f   | ```mc-biome-viewer-forward-x```     | Move the camera to the right one chunk             |
| b   | ```mc-biome-viewer-backward-x```    | Move the camera left one chunk                     |
| n   | ```mc-biome-viewer-backward-y```     | Move the camera downwards one chunk                  |
| p   | ```mc-biome-viewer-forward-y```    | Move the camera upwards one chunk                |
| j   | ```mc-biome-viewer-centre-camera``` | Centre the camera at the prompted world coordinate |

You can move from biome to biome using the usual text movement commands and information about the chunk the cursor is on will be displayed in a label in the bottom left corner (see above).

## Installation

The easiest way is with ![quelpa-use-package](https://github.com/quelpa/quelpa-use-package):

```lisp
(use-package mc-biome-viewer
    :ensure nil
    :quelpa (mc-biome-viewer :fetcher github :repo "LaurenceWarne/mc-biome-viewer")
    ;; Example configuration
    :config
    (setq mc-biome-viewer-column-chunks-in-camera 64)
    (puthash "ice plains" '(:foreground "silver") mc-biome-viewer-biome-to-face-map))
```
