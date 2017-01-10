;;; ivy-rich.el --- More friendly display transformer for ivy.

;; Copyright (C) 2016 Yevgnen Koh

;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Package-Requires: ((emacs "24.4") (ivy "0.8.0"))
;; Package-Version: 20161228.2022
;; Version: 0.0.1
;; Keywords: ivy

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

;; More friendly interface (display transformer) for ivy.
;; Usage:
;; (require 'ivy-rich)
;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
;;
;; See documentation on https://github.com/yevgnen/ivy-rich.

;;; Code:

(require 'ivy)
(require 'subr-x)

(declare-function projectile-project-name "projectile")
(declare-function projectile-project-p "projectile")
(declare-function projectile-project-root "projectile")

;;; ivy-switch-buffer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup ivy-rich nil
  "More friendly interface (display transformer) for ivy."
  :group 'ivy)

(defcustom ivy-rich-switch-buffer-name-max-length
  32
  "Max length of buffer name.

For better user experience, the max length should be set to loose to
hold the buffer name."
  :type 'integer)

(defcustom ivy-rich-switch-buffer-mode-max-length
  18
  "Max length of mode name.

For better user experience, the max length should be set to loose to
hold the mode name."
  :type 'integer)
(defcustom ivy-rich-switch-buffer-project-max-length
  15
  "Max length of project name.

For better user experience, the max length should be set to loose
to hold the project name."
  :type 'integer)

(defcustom ivy-rich-switch-buffer-delimiter
  ""
  "Delimiter between columns."
  :type 'string)

(defcustom ivy-rich-switch-buffer-align-virtual-buffer
  nil
  "Whether to align virtual buffers just as true buffers or not."
  :type 'boolean)

(defvar ivy-rich-switch-buffer-buffer-size-length 7)

(defun ivy-rich-switch-buffer-pad (str len &optional left)
  "Use space to pad STR to LEN of length.

When LEFT is not nil, pad from left side."
  (if (< (length str) len)
      (if left
          (concat (make-string (- len (length str)) ? ) str)
        (concat str (make-string (- len (length str)) ? )))
    str))

(defun ivy-rich-switch-buffer-mode (mode)
  "Transform MODE name to a more friendly format."
  (capitalize
   (replace-regexp-in-string "-" " " (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))))

(defun ivy-rich-switch-buffer-user-buffer-p (buffer)
  "Check whether BUFFER-NAME is a user buffer."
  (let ((buffer-name
         (if (stringp buffer)
             buffer
           (buffer-name buffer))))
    (not (string-match "^\\*" buffer-name))))

(defun ivy-rich-switch-buffer-excluded-modes-p (modes)
  "Check whether major mode of current buffer is excluded in MODES."
  (not (memq major-mode modes)))

(defun ivy-rich-switch-buffer-shorten-path (file)
  "Shorten the path of FILE.

For example, a path /a/b/c/d/e/f.el will be shortened to /a/…/e/f.el."
  (replace-regexp-in-string "\\/?.+?\\/\\(.+\\)\\/.+?\\/.*" "…" file nil nil 1))

;;;###autoload
(defun ivy-rich-switch-buffer-transformer (str)
  "Transform STR to more readable format.

Currently the transformed format is

| Buffer name | Buffer indicators | Major mode | Project | Path (Based on project root) |."
  (let ((buf (get-buffer str)))
    (if buf
        (with-current-buffer buf
          (let* (;; Indicators
                 (modified (propertize (if (and (buffer-modified-p)
                                                (ivy-rich-switch-buffer-excluded-modes-p '(dired-mode shell-mode))
                                                (ivy-rich-switch-buffer-user-buffer-p str))
                                           "*"
                                         "")
                                       'face 'error))
                 (readonly (propertize (if (and buffer-read-only
                                                (ivy-rich-switch-buffer-user-buffer-p str))
                                           "!"
                                         "")
                                       'face 'error))
                 (process (propertize (if (get-buffer-process (current-buffer))
                                          "&"
                                        "")
                                      'face 'error))
                 (indicator (ivy-rich-switch-buffer-pad (format "%s%s%s" readonly modified process) 3))
                 ;; Size
                 (size (buffer-size))
                 (size (cond
                        ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
                        ((> size 1000) (format "%.1fk " (/ size 1000.0)))
                        (t (format "%d " size))))
                 (size (ivy-rich-switch-buffer-pad size ivy-rich-switch-buffer-buffer-size-length t))
                 ;; Buffer name
                 (name (ivy-rich-switch-buffer-pad str ivy-rich-switch-buffer-name-max-length))
                 (name (propertize name 'face 'ivy-modified-buffer))
                 ;; Major mode
                 (mode (ivy-rich-switch-buffer-pad (ivy-rich-switch-buffer-mode major-mode) ivy-rich-switch-buffer-mode-max-length))
                 (mode (propertize mode 'face 'warning))
                 ;; Project
                 (project
                  (if (not (bound-and-true-p projectile-mode))
                      ""
                    (let ((project (projectile-project-name)))
                      (propertize (ivy-rich-switch-buffer-pad
                                   (if (string= project "-")
                                       ""
                                     project)
                                   ivy-rich-switch-buffer-project-max-length)
                                  'face 'success))))
                 (project-home (if (or (string-empty-p project)
                                       (not (projectile-project-p)))
                                   ""
                                 (file-truename (projectile-project-root))))
                 ;; Path
                 (path-max-length (- (window-width (minibuffer-window))
                                     ivy-rich-switch-buffer-name-max-length
                                     (length indicator)
                                     ivy-rich-switch-buffer-buffer-size-length
                                     ivy-rich-switch-buffer-mode-max-length
                                     ivy-rich-switch-buffer-project-max-length
                                     ;; Fixed the unexpected wrapping in terminal
                                     1))
                 (path (file-truename (or (buffer-file-name) default-directory)))
                 (path (if (string-empty-p project)
                           path
                         (substring-no-properties path (length project-home))))
                 (path (if (> (length path) path-max-length)
                           (ivy-rich-switch-buffer-shorten-path path)
                         path))
                 (path (ivy-rich-switch-buffer-pad path path-max-length))
                 (display (format "%s%s%s%s%s%s%s%s%s%s"
                                  name
                                  size ivy-rich-switch-buffer-delimiter
                                  indicator ivy-rich-switch-buffer-delimiter
                                  mode ivy-rich-switch-buffer-delimiter
                                  project ivy-rich-switch-buffer-delimiter
                                  path)))
            display))
      (if (and (eq ivy-virtual-abbreviate 'full)
               ivy-rich-switch-buffer-align-virtual-buffer)
          (let* (;; File name
                 (filename (file-name-nondirectory (expand-file-name str)))
                 (filename (ivy-rich-switch-buffer-pad filename
                                                       (+ ivy-rich-switch-buffer-name-max-length
                                                          3  ; width of indicators
                                                          ivy-rich-switch-buffer-buffer-size-length
                                                          ivy-rich-switch-buffer-mode-max-length
							  (* 3 (length ivy-rich-switch-buffer-delimiter))
                                                          (if (not (bound-and-true-p projectile-mode)) 0 ivy-rich-switch-buffer-project-max-length))))
                 (filename (propertize filename 'face 'ivy-virtual))
                 ;; Path
                 (path (file-name-directory str))
                 (path (if (> (length path) (- (window-width (minibuffer-window)) (length filename)))
                           (ivy-rich-switch-buffer-shorten-path path)
                         path))
                 (path (propertize path 'face 'ivy-virtual)))
            (format "%s%s%s"
                    filename
                    ivy-rich-switch-buffer-delimiter

                    (ivy-rich-switch-buffer-pad path (- (window-width)
                                                        (length filename)
                                                        ;; Fixed the unexpected wrapping in terminal
                                                        1))))
        str))))

(provide 'ivy-rich)

;;; ivy-rich.el ends here
