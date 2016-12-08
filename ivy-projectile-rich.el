;;; ivy-projectile-rich.el --- More friendly interface for `counsel-projectile'.

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

;; More friendly interface for `counsel-projectile'.
;;
;; See documentation on https://github.com/yevgnen/ivy-rich.

;;; Code:


(require 'ivy)
(require 'subr-x)
(require 'projectile)
(require 'counsel-projectile)

(require 'ivy-switch-buffer-rich)

(defun ivy-projectile-vc-backend (dir)
  (cond ((file-exists-p (expand-file-name ".git" dir))
         "git")
        (t "")))

(defcustom ivy--projectile-project-name-max-length
  20
  "Max length of mode name.

For better user experience, the max length should be set to loose to
hold the mode name."
  :type 'integer)

(defcustom ivy--projectile-vc-max-length
  8
  "Max length of mode name.

For better user experience, the max length should be set to loose to
hold the mode name."
  :type 'integer)

(defun ivy-projectile-switch-project-transformer (str)
  "Transform STR to more readable format.

Currently the transformed format is

| Project name | path."
  (let* (;; Project
         (default-directory str)
         (project (projectile-project-name))
         (project (propertize (ivy--switch-buffer-pad
                               (if (string= project "-")
                                   ""
                                 project)
                               ivy--switch-buffer-project-max-length)
                              'face 'success))
         (project (ivy--switch-buffer-pad project ivy--projectile-project-name-max-length))
         ;; VC
         (vc (ivy--switch-buffer-pad
              (capitalize (ivy-projectile-vc-backend str))
              ivy--projectile-vc-max-length))
         (vc (propertize vc 'face 'warning))
         ;; Path
         (path-max-length (- (window-width (minibuffer-window))
                             ivy--switch-buffer-name-max-length
                             ivy--switch-buffer-mode-max-length
                             ivy--switch-buffer-project-max-length))
         (path (expand-file-name str))
         (display (format "%s%s%s%s%s"
                          project ivy--switch-buffer-delimiter
                          vc ivy--switch-buffer-delimiter
                          path)))
    display))

(ivy-set-display-transformer
 'counsel-projectile-switch-to-buffer 'ivy-switch-buffer-rich-transformer)

(ivy-set-display-transformer
 'counsel-projectile-switch-project 'ivy-projectile-switch-project-transformer)

(provide 'ivy-projectile-rich)

;;; ivy-projectile-rich.el ends here
