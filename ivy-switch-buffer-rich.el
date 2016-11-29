;;; ivy-switch-buffer-rich.el --- More friendly interface for `ivy-switch-buffer'.

;; Copyright (C) 2016 Yevgnen Koh

;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Package-Requires: ((emacs "24.4") (ivy "0.8.0") (projectile "0.15.0"))
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

;; More friendly interface for `ivy-switch-buffer'.
;;
;; See documentation on https://github.com/yevgnen/ivy-rich.

;;; Code:


(require 'ivy)
(require 'subr-x)
(require 'projectile)

(defgroup ivy-switch-buffer-rich nil
  "Readible format for `ivy-switch-buffer'."
  :group 'ivy-rich)

(defcustom ivy--switch-buffer-name-max-length
  40
  "Max length of buffer name.

For better user experience, the max length should be set to loose to
hold the buffer name."
  :type 'integer)

(defcustom ivy--switch-buffer-mode-max-length
  18
  "Max length of mode name.

For better user experience, the max length should be set to loose to
hold the mode name."
  :type 'integer)
(defcustom ivy--switch-buffer-project-max-length
  15
  "Max length of project name.

For better user experience, the max length should be set to loose
to hold the project name."
  :type 'integer)

(defcustom ivy--switch-buffer-delimiter
  ""
  "Delimiter between columns."
  :type 'string)

(defun ivy--switch-buffer-pad (str len)
  "Use space to pad STR to LEN of length."
  (if (< (length str) len)
      (concat str (make-string (- len (length str)) ? ))
    str))

(defun ivy--switch-buffer-mode (mode)
  "Transform MODE name to a more friendly format."
  (capitalize
   (replace-regexp-in-string "-" " " (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))))

(defun ivy--switch-buffer-user-buffer-p (buffer)
  "Check whether BUFFER-NAME is a user buffer."
  (let ((buffer-name
         (if (stringp buffer)
             buffer
           (buffer-name buffer))))
    (not (string-match "^\\*" buffer-name))))

(defun ivy--switch-buffer-excluded-modes-p (modes)
  "Check whether major mode of current buffer is excluded in MODES."
  (not (memq major-mode modes)))

(defun ivy--switch-buffer-shorten-path (file)
  "Shorten the path of FILE.

For example, a path /a/b/c/d/e/f.el will be shortened to /a/…/e/f.el."
  (replace-regexp-in-string "\\/?.+?\\/\\(.+\\)\\/.+?\\/.*" "…" file nil nil 1))

(defun ivy-switch-buffer-rich-transformer (str)
  "Transform STR to more readable format.

Currently the transformed format is

| Buffer name | Buffer indicators | Major mode | Project | Path (Based on project root) |."
  (let ((buf (get-buffer str)))
    (if buf
        (with-current-buffer buf
          (let* (;; Indicators
                 (modified (propertize (if (and (buffer-modified-p)
                                                (ivy--switch-buffer-excluded-modes-p '(dired-mode shell-mode))
                                                (ivy--switch-buffer-user-buffer-p str))
                                           "*"
                                         "")
                                       'face 'error))
                 (readonly (propertize (if (and buffer-read-only
                                                (ivy--switch-buffer-user-buffer-p str))
                                           "!"
                                         "")
                                       'face 'error))
                 (process (propertize (if (get-buffer-process (current-buffer))
                                          "&"
                                        "")
                                      'face 'error))
                 (indicator (ivy--switch-buffer-pad (format "%s%s%s" readonly modified process) 3))
                 ;; Buffer name
                 (name (ivy--switch-buffer-pad str ivy--switch-buffer-name-max-length))
                 (name (propertize name 'face 'ivy-modified-buffer))
                 ;; Major mode
                 (mode (ivy--switch-buffer-pad (ivy--switch-buffer-mode major-mode) ivy--switch-buffer-mode-max-length))
                 (mode (propertize mode 'face 'warning))
                 ;; Project
                 (project (projectile-project-name))
                 (project (propertize (ivy--switch-buffer-pad
                                       (if (string= project "-")
                                           ""
                                         project)
                                       ivy--switch-buffer-project-max-length)
                                      'face 'success))
                 (project-home (if (or (string-empty-p project)
                                       (not (projectile-project-p)))
                                   ""
                                 (file-truename (projectile-project-root))))
                 ;; Path
                 (path-max-length (- (window-width (minibuffer-window))
                                     ivy--switch-buffer-name-max-length
                                     (length indicator)
                                     ivy--switch-buffer-mode-max-length
                                     ivy--switch-buffer-project-max-length))
                 (path (file-truename (or (buffer-file-name) default-directory)))
                 (path (if (string-empty-p project)
                           path
                         (substring-no-properties path (length project-home))))
                 (path (if (> (length path) path-max-length)
                           (ivy--switch-buffer-shorten-path path)
                         path))
                 (path (ivy--switch-buffer-pad path path-max-length))
                 (display (format "%s%s%s%s%s%s%s%s"
                                  name indicator ivy--switch-buffer-delimiter
                                  mode ivy--switch-buffer-delimiter
                                  project ivy--switch-buffer-delimiter
                                  path)))
            display))
      str)))

(ivy-set-display-transformer
 'ivy-switch-buffer 'ivy-switch-buffer-rich-transformer)

(provide 'ivy-switch-buffer-rich)

;;; ivy-switch-buffer-rich.el ends here
