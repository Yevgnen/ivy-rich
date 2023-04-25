;;; ivy-rich.el --- More friendly display transformer for ivy -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Yevgnen Koh

;; Author: Yevgnen Koh <wherejoystarts@gmail.com>
;; Homepage: https://github.com/Yevgnen/ivy-rich
;; Package-Requires: ((emacs "25.1") (ivy "0.13.0"))
;; Version: 0.1.7
;; Keywords: convenience, ivy

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; More friendly interface (display transformer) for ivy.
;; Usage:
;; (require 'ivy-rich)
;; (ivy-rich-mode 1)
;;
;; See documentation on https://github.com/yevgnen/ivy-rich.

;;; Code:

(require 'cl-lib)
(require 'ivy)
(require 'subr-x)

(eval-when-compile
  (require 'package)
  (require 'bookmark)
  (require 'project))

(declare-function projectile-project-name "ext:projectile")
(declare-function projectile-project-p "ext:projectile")
(declare-function projectile-project-root "ext:projectile")

(defgroup ivy-rich nil
  "More friendly interface (display transformer) for ivy."
  :group 'ivy)

(defcustom ivy-rich-display-transformers-list
  '(ivy-switch-buffer
    (:columns
     ((ivy-switch-buffer-transformer (:width 0.35))
      (ivy-rich-switch-buffer-size (:width 7))
      (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
      (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
      (ivy-rich-switch-buffer-project (:width 0.18 :face success))
      (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
     :predicate
     (lambda (cand) (get-buffer cand)))
    counsel-find-file
    (:columns
     ((ivy-read-file-transformer)
      (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face))))
    counsel-M-x
    (:columns
     ((counsel-M-x-transformer (:width 0.4))
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
    counsel-describe-function
    (:columns
     ((counsel-describe-function-transformer (:width 0.4))
      (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
    counsel-describe-variable
    (:columns
     ((counsel-describe-variable-transformer (:width 0.4))
      (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
    counsel-recentf
    (:columns
     ((ivy-rich-candidate (:width 0.8))
      (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
    counsel-bookmark
    (:columns ((ivy-rich-candidate (:width 0.3))
               (ivy-rich-bookmark-type)
               (ivy-rich-bookmark-info)))
    package-install
    (:columns
     ((ivy-rich-candidate (:width 30))
      (ivy-rich-package-version (:width 16 :face font-lock-comment-face))
      (ivy-rich-package-archive-summary (:width 7 :face font-lock-builtin-face))
      (ivy-rich-package-install-summary (:face font-lock-doc-face)))))
  "Definitions for ivy-rich transformers.

The definitions should be in the following plist format

'(CMD-1 TRANSFORM-PROPS-1
  ...
  CMD-N TRANSFORM-PROPS-N)

A transformer named `ivy-rich--CMD-transformer' is built for each
command CMD.

CMD should be an ivy command, which is typically a return value
of `ivy-read'.

TRANSFORM-PROPS are properties for defining transformer in plist
format, i.e.

(:columns (COLUMN-FN1 (KEY1 VALUE1 KEY2 VALUE2 ...))
                  (COLUMN-FN2 (KEY1 VALUE1 KEY2 VALUE2 ...))
        :predicate PREDICATE-FN)

COLUMN-FN is a function which takes the completion candidate as
single argument and it should return a transformed string. This
function should return an empty string \"\" instead of nil when
the transformed string is empty.

The KEY-VALUE pairs are custom properties in plist format for the
corresponding column definition. Current supported keys are
:width, :face and :align.

A integer (or float) :width value indicates the max
width (percentage) of current column. For better displaying, you
should set :width to some reasonable values. If :width is a
function, the transformed string is again passed to it and it
should return a new string with properly processed width.

:face is the face property for the column string. :align
should be set to 'left (default if not given) or 'right to
indicate where to pad extra spaces to the columns for alignment.

The value of :delimiter should be a string for current
transformer. If not given, the default is a single space.

If :predicate is provide, it should be a function which takes the
completion candidate as single argument. A candidate with nil
predication will not be transformed.

It is possible to set TRANSFORM-PROPS to a pre-defined
transformer, e.g.

(...
counsel-M-x
(:columns
 ((counsel-M-x-transformer (:width 40))
  (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))

execute-extended-command		; reuse transformer built
ivy-rich--counsel-M-x-transformer	; for `counsel-M-x'
...)

`execute-extended-command' is set to used `counsel-M-x''s
transformer. This is useful if one want to reuse transformers
without duplicating definitions.

Note that you may need to disable and enable the `ivy-rich-mode'
again to make this variable take effect.")

;;; User convenience functions
;; Helper functions for user profile configuration

(defun ivy-rich-modify-column (cmd column attrs)
  "Customize the CMD transformer's properties for a specific COLUMN.
Each key-value pair in ATTRS is put into the property list for the column.
Existing properties for the column are left unchanged.

The COLUMN has to be exist. You can't modify a non-exist column
because ivy-rich doesn't know how to order new columns.

Usage:

(ivy-rich-modify-column 'ivy-switch-buffer
                        'ivy-rich-switch-buffer-major-mode
                        '(:width 20 :face error))"
  (if (cl-evenp (length attrs))
      (let* ((trans (plist-get ivy-rich-display-transformers-list cmd))
             (columns (plist-get trans :columns))
             (props (cadr (assq column columns))))
        (unless props
          (error "Can not modify non-exist column"))
        (while attrs
          (setq props (plist-put props (pop attrs) (pop attrs))))
        (setcdr (assq column columns) (list props))
        (setq ivy-rich-display-transformers-list
              (plist-put ivy-rich-display-transformers-list
                         cmd
                         (plist-put trans :columns columns)))
        (ivy-rich-set-display-transformer nil))

    (error "Column key-value attributes must be in pairs")))

(defun ivy-rich-modify-columns (cmd column-list)
  "Customize the CMD transformer's properties for a COLUMN-LIST.
This is a convenience function that calls `ivy-rich-modify-column' for each item
in COLUMN-LIST, allowing multiple columns to be modified for a transformer.
Each item in COLUMN-LIST is a two-item list comprised of a column and list
of attribute key-value pairs.

Usage:

(ivy-rich-modify-columns
 'ivy-switch-buffer
 '((ivy-rich-switch-buffer-size (:align right))
   (ivy-rich-switch-buffer-major-mode (:width 20 :face error))))"
  (while column-list
    (if-let ((column (caar column-list))
             (attrs (cadar column-list)))
        (prog1
            (ivy-rich-modify-column cmd column attrs)
          (setq column-list (cdr column-list)))
      (error "Column/attributes are incorrectly specified"))))

(defun ivy-rich-set-columns (cmd column-list)
  "Set the CMD transformer's properties for a COLUMN-LIST.

The :columns of the given command will be replaced by COLUMN-LIST.
Each item in COLUMN-LIST is a two-item list comprised of a column and list
of attribute key-value pairs.

Usage:

(ivy-rich-set-columns
 'counsel-recentf
 '((file-name-nondirectory
    (:width 0.2))
   (ivy-rich-candidate
    (:width 0.6))))"
  (let* ((trans (plist-get ivy-rich-display-transformers-list cmd)))
    (setq ivy-rich-display-transformers-list
          (plist-put ivy-rich-display-transformers-list
                     cmd
                     (plist-put trans :columns column-list)))
    (ivy-rich-set-display-transformer nil)))

;; Common Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'ivy-rich-candidate 'identity)

(defun ivy-rich-empty-p (str)
  (or (null str)
      (string-empty-p (string-trim str))))

(defun ivy-rich-normalize-width (str len &optional right-aligned)
  "Normalize the width of a string.

If the length of STR is smaller than LEN, the string is padded to
right aligned if RIGHT-ALIGNED is not nil and is padded to left
otherwise.

If the lenght of STR is larger that LEN, the string is truncated
using …."
  (let ((str-len (string-width str)))
    (cond ((< str-len len)
           (if right-aligned
               (concat (make-string (- len str-len) ? ) str)
             (concat str (make-string (- len str-len) ? ))))
          ((<= len (- str-len)) "")
          ((> str-len len)
           (truncate-string-to-width str len 0 nil "…"))
          (t str))))

(defun ivy-rich-minibuffer-width (width)
  (cond ((and (integerp width)
              (> width 0))
         width)
        ((and (floatp width)
              (> width 0.0)
              (<= width 1.0))
         (floor (* (window-width (minibuffer-window)) width)))
        (t (user-error "Width should be positive integer or float int (0.0, 1.0]"))))

;; Supports for `ivy-switch-buffer' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defcustom ivy-rich-path-style
  'relative
  "File path style.

When set to 'full or 'absolute, absolute path will be used.
When set to 'abbrev or 'abbreviate, abbreviated will be used. This
may not affect remote files since `abbreviate-file-name' does not
take care of them.
When set to 'relative or any other value, path relative to project
home will be used."
  :type 'symbol)

(defcustom ivy-rich-parse-remote-buffer
  t
  "Whether to parse remote files.

When `nil', only basic info of remote buffers, like buffer size,
major mode, etc. will be parsed, otherwise, all info inculding
project details, file path will be parsed.

If you have performance issue when accessing tramp files, set
this to `nil'."
  :type 'boolean)

(defcustom ivy-rich-parse-remote-file-path
  nil
  "Whether `ivy-rich-path-style' should take care of remote file.

When `nil', always show absolute path of remote files,
otherwise, treat remote files as local files.

Sometimes when you are editing files with same names and same
directory structures in local and remote machines, setting this
option to `nil' would make the candidates easier to be
distinguished.

Note that this variable takes effect only when
`ivy-rich-parse-remote-buffer' is set to `t'."
  :type 'boolean)

(defun ivy-rich-switch-buffer-user-buffer-p (buffer)
  "Check whether BUFFER-NAME is a user buffer."
  (let ((buffer-name
         (if (stringp buffer)
             buffer
           (buffer-name buffer))))
    (not (string-match "^\\*" buffer-name))))

(defun ivy-rich-switch-buffer-shorten-path (file len)
  "Shorten the path of FILE until the length of FILE <= LEN.

For example, a path /a/b/c/d/e/f.el will be shortened to
   /a/…/c/d/e/f.el
or /a/…/d/e/f.el
or /a/…/e/f.el
or /a/…/f.el."
  (if (> (length file) len)
      (let ((new-file (replace-regexp-in-string "/?.+?/\\(\\(…/\\)?.+?\\)/.*" "…" file nil nil 1)))
        (if (string= new-file file)
            file
          (ivy-rich-switch-buffer-shorten-path new-file len)))
    file))

(defun ivy-rich--local-values (buffer args)
  (let ((buffer (get-buffer buffer)))
    (if (listp args)
        (mapcar #'(lambda (x) (buffer-local-value x buffer)) args)
      (buffer-local-value args buffer))))

(defun ivy-rich-switch-buffer-buffer-name (candidate)
  candidate)

(defun ivy-rich-switch-buffer-indicators (candidate)
  (let* ((buffer (get-buffer candidate))
         (process-p (get-buffer-process buffer)))
    (cl-destructuring-bind
        (filename directory read-only)
        (ivy-rich--local-values candidate '(buffer-file-name default-directory buffer-read-only))
      (let ((modified (if (and (buffer-modified-p buffer)
                               (null process-p)
                               (ivy-rich-switch-buffer-user-buffer-p candidate))
                          "*"
                        ""))
            (readonly (if (and read-only (ivy-rich-switch-buffer-user-buffer-p candidate))
                          "!"
                        ""))
            (process (if process-p
                         "&"
                       ""))
            (remote (if (file-remote-p (or filename directory))
                        "@"
                      "")))
        (format "%s%s%s%s" remote readonly modified process)))))

(defun ivy-rich-switch-buffer-size (candidate)
  (with-current-buffer
      (get-buffer candidate)
    (let ((size (buffer-size)))
      (cond
       ((> size 1000000) (format "%.1fM " (/ size 1000000.0)))
       ((> size 1000) (format "%.1fk " (/ size 1000.0)))
       (t (format "%d " size))))))

(defun ivy-rich-switch-buffer-major-mode (candidate)
  (capitalize
   (replace-regexp-in-string
    "-"
    " "
    (replace-regexp-in-string
     "-mode"
     ""
     (symbol-name (ivy-rich--local-values candidate 'major-mode))))))

(defun ivy-rich--switch-buffer-directory (candidate)
      "Return directory of file visited by buffer named CANDIDATE, or nil if no file."
      (let* ((buffer (get-buffer candidate))
             (fn (buffer-file-name buffer)))
        ;; if valid filename, i.e. buffer visiting file:
        (if fn
            ;; return containing directory
            (file-name-directory fn)
          ;; else if mode explicitly offering list-buffers-directory, return that; else nil.
          ;; buffers that don't explicitly visit files, but would like to show a filename,
          ;; e.g. magit or dired, set the list-buffers-directory variable
          (buffer-local-value 'list-buffers-directory buffer))))

(defvar ivy-rich--project-root-cache
  (make-hash-table :test 'equal)
  "Hash-table caching each file's project for
`ivy-rich-switch-buffer-root'.

The cache can is enabled when `ivy-rich-project-root-cache-mode'
is enabled and cleared when the mode is disabled. Additionally,
buffers are removed from the cached when killd.

The cache can be cleared manually by calling
`ivy-rich-clear-project-root-cache'.")

(defun ivy-rich-clear-project-root-cache ()
  "Resets `ivy-rich--project-root-cache'."
  (interactive)
  (clrhash ivy-rich--project-root-cache))

(defun ivy-rich-switch-buffer-root-lookup (candidate dir)
  (unless (or (and (file-remote-p dir)
                     (not ivy-rich-parse-remote-buffer))
                ;; Workaround for `browse-url-emacs' buffers , it changes
                ;; `default-directory' to "http://" (#25)
                (string-match "https?://" dir))
      (cond ((bound-and-true-p projectile-mode)
             (let ((project (or (ivy-rich--local-values
                                 candidate 'projectile-project-root)
                                (projectile-project-root dir))))
               (unless (string= project "-")
                 project)))
            ((require 'find-file-in-project nil t)
             (let ((default-directory dir))
               (ffip-project-root)))
            ((require 'project nil t)
             (when-let ((project (project-current nil dir)))
               (car (project-roots project)))))))

(defun ivy-rich-switch-buffer-root (candidate)
  (when-let ((dir (ivy-rich--switch-buffer-directory candidate)))
    (let ((cached-value (if ivy-rich-project-root-cache-mode
                            (gethash dir ivy-rich--project-root-cache 'not-found)
                          'not-found)))
      (if (not (eq cached-value 'not-found))
          cached-value
        (let ((value (ivy-rich-switch-buffer-root-lookup candidate dir)))
          (when ivy-rich-project-root-cache-mode
            (puthash dir value ivy-rich--project-root-cache))
          value)))))

(defun ivy-rich-project-root-cache-kill-buffer-hook ()
  "This hook is used to remove buffer from
`ivy-rich--project-root-cache' when they are killed."
  (remhash (ivy-rich--switch-buffer-directory
            (buffer-name (current-buffer)))
           ivy-rich--project-root-cache))

(defun ivy-rich-switch-buffer-project (candidate)
  (file-name-nondirectory
   (directory-file-name
    (or (ivy-rich-switch-buffer-root candidate) ""))))

(defun ivy-rich--switch-buffer-root-and-filename (candidate)
  (when-let ((root (ivy-rich-switch-buffer-root candidate))
             (dir (ivy-rich--switch-buffer-directory candidate)))
    (when (bound-and-true-p projectile-mode)
      (setq dir (or (file-name-directory
                     (or (ivy-rich--local-values
                          candidate 'buffer-file-truename)
                         ""))
                    (file-truename dir))))
    (cons (expand-file-name root) (expand-file-name dir))))

(defun ivy-rich-switch-buffer-path (candidate)
  (if-let ((result (ivy-rich--switch-buffer-root-and-filename candidate)))
      (cl-destructuring-bind (root . filename) result
        (cond
         ;; Case: absolute
         ((or (memq ivy-rich-path-style '(full absolute))
              (and (null ivy-rich-parse-remote-file-path)
                   (or (file-remote-p root))))
          (or filename root))
         ;; Case: abbreviate
         ((memq ivy-rich-path-style '(abbreviate abbrev))
          (abbreviate-file-name (or filename root)))
         ;; Case: relative
         ((or (eq ivy-rich-path-style 'relative)
              t)	    ; make 'relative default
          (if (and filename root)
              (let ((relative-path (string-remove-prefix root filename)))
                (if (string= relative-path candidate)
                    (file-name-as-directory
                     (file-name-nondirectory
                      (directory-file-name (file-name-directory filename))))
                  relative-path))
            ""))))
    ""))


;; Supports for `counsel-find-file'
(defun ivy-rich-counsel-find-file-truename (candidate)
  (let ((type (car (ignore-errors (file-attributes (directory-file-name (expand-file-name candidate ivy--directory)))))))
    (if (stringp type)
        (concat "-> " (expand-file-name type ivy--directory))
      "")))

;; Supports for `counsel-M-x', `counsel-describe-function', `counsel-describe-variable'
(defun ivy-rich-counsel-function-docstring (candidate)
  (let* (
         ;; Stole from:
         ;; https://github.com/minad/marginalia/blob/51f750994aaa0b6798d97366acfb0d397639af66/marginalia.el#L355
         (regex (rx bos
                    (1+ (seq (? "This function has ")
                             (or ":before" ":after" ":around" ":override"
                                 ":before-while" ":before-until" ":after-while"
                                 ":after-until" ":filter-args" ":filter-return")
                             " advice: " (0+ nonl) "\n"))
                    "\n"))
         (doc (replace-regexp-in-string
               regex
               ""
               (or (ignore-errors (documentation (intern-soft candidate))) ""))))
    (if (string-match "^\\(.+\\)\\([\r\n]\\)?" doc)
        (setq doc (match-string 1 doc))
      "")))

(defun ivy-rich-counsel-variable-docstring (candidate)
  (let ((doc (documentation-property
              (intern-soft candidate) 'variable-documentation)))
    (if (and doc (string-match "^\\(.+\\)\\([\r\n]\\)?" doc))
        (setq doc (match-string 1 doc))
      "")))

;; Supports for `counsel-recentf'
(defun ivy-rich-file-last-modified-time (candidate)
  (let ((candidate (expand-file-name candidate ivy--directory)))
    (if (file-remote-p candidate)
        "?"
      (format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 (file-attributes candidate))))))

;; Supports for `counsel-bookmark'
(defun ivy-rich-bookmark-value (candidate key)
  (cdr (assoc key (cdr (assoc candidate bookmark-alist)))))

(defun ivy-rich-bookmark-filename (candidate)
  (ivy-rich-bookmark-value candidate 'filename))

(defun ivy-rich-bookmark-handler-props (candidate)
  (let ((handler (ivy-rich-bookmark-value candidate 'handler)))
    (unless (null handler)
      (list (upcase (car (cl-remove-if (lambda (x)
                                         (or (string= "bookmark" x)
                                             (string= "jump" x)))
                                       (split-string (symbol-name handler) "-"))))
            'font-lock-keyword-face))))

(defun ivy-rich-bookmark-propertize-type (string face)
  (propertize (format "%-8.8s" string) 'face face))

(defun ivy-rich-bookmark-type (candidate)
  (let ((filename (ivy-rich-bookmark-filename candidate)))
    (apply #'ivy-rich-bookmark-propertize-type
           (cond ((null filename) (or (ivy-rich-bookmark-handler-props candidate)
                                      '("NOFILE" warning)))
                 ((file-remote-p filename) '("REMOTE" mode-line-buffer-id))
                 ((not (file-exists-p filename)) (or (ivy-rich-bookmark-handler-props candidate)
                                                     '("NOTFOUND" error)))
                 ((file-directory-p filename) '("DIRED" warning))
                 (t '("FILE" success))))))

(defun ivy-rich-bookmark-info (candidate)
  (let ((filename (ivy-rich-bookmark-filename candidate)))
    (cond (filename
           (cond ((null filename)
                  "")
                 ((file-remote-p filename)
                  candidate)
                 ((file-exists-p filename)
                  (file-truename filename))
                 (t filename))))))

;; Supports for `counsel-projectile'
;; Possible setup:
;; counsel-projectile-switch-project
;; (:columns
;;  ((ivy-rich-counsel-projectile-switch-project-project-name (:width 30 :face success))
;;   (ivy-rich-candidate)))
(defun ivy-rich-counsel-projectile-switch-project-project-name (candidate)
  (or (projectile-project-name candidate) ""))

;; Supports for `package-install'
(defun ivy-rich-package-install-summary (candidate)
  (let ((package-desc (cadr (assoc-string candidate package-archive-contents))))
    (if package-desc (package-desc-summary package-desc) "")))

(defun ivy-rich-package-archive-summary (candidate)
  (let ((package-arch (cadr (assoc-string candidate package-archive-contents))))
    (if package-arch (package-desc-archive package-arch) "")))

(defun ivy-rich-package-version (candidate)
  (let ((package-vers (cadr (assoc-string candidate package-archive-contents))))
    (if package-vers (package-version-join (package-desc-version package-vers)) "")))

;; Definition of `ivy-rich-mode' ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ivy-rich--original-display-transformers-list nil)  ; Backup list

(defun ivy-rich-format-column (candidate column)
  (let* ((fn (car column))
         (props (cadr column))
         (width (plist-get props :width))
         (align (plist-get props :align))
         (face (plist-get props :face))
         (formated (funcall fn candidate)))
    (when width
      (if (functionp width)
          (setq formated (funcall width formated))
        (if (floatp width)
            (setq width (floor (* (window-width (minibuffer-window)) width))))
        (setq formated (ivy-rich-normalize-width formated width (eq align 'right)))))
    (if face
        (setq formated (propertize formated 'face face)))
    formated))

(defun ivy-rich-format (candidate columns &optional delimiter)
  (mapconcat
   (lambda (column)
     (or (ivy-rich-format-column candidate column) ""))
   columns
   delimiter))

(defun ivy-rich-backup-transformer (cmd)
  (setq ivy-rich--original-display-transformers-list
        (plist-put ivy-rich--original-display-transformers-list
                   cmd
                   (alist-get cmd ivy--display-transformers-alist))))

(defun ivy-rich-restore-transformer (cmd)
  (setq ivy--display-transformers-alist
        (ivy--alist-set 'ivy--display-transformers-alist
                        cmd
                        (plist-get ivy-rich--original-display-transformers-list cmd))))

(defun ivy-rich-build-transformer (cmd transformer-props)
  (if (functionp transformer-props)
      transformer-props
    (defalias (intern (format "ivy-rich--%s-transformer" (symbol-name cmd)))
      (lambda  (candidate)
        (let ((columns (plist-get transformer-props :columns))
              (predicate-fn (or (plist-get transformer-props :predicate) (lambda (_) t)))
              (delimiter (or (plist-get transformer-props :delimiter) " ")))
          (if (and predicate-fn
                   (not (funcall predicate-fn candidate)))
              candidate
            (ivy-rich-format candidate columns delimiter)))))))

(defun ivy-rich-set-display-transformer (backup)
  (cl-loop for (cmd transformer-props) on ivy-rich-display-transformers-list by 'cddr do
           (let* ((cmd-string (symbol-name cmd))
                  (package (if (string-match "^\\(swiper\\|counsel\\)" cmd-string)
                               (match-string 1 cmd-string))))
             (if package
                 (require (intern package)))  ; NOTE: Need to load the original transformer
             (if backup
                 (ivy-rich-backup-transformer cmd))
             (ivy-set-display-transformer cmd (ivy-rich-build-transformer cmd transformer-props)))))

(defun ivy-rich-unset-display-transformer ()
  (cl-loop for (cmd _transformer-fn) on ivy-rich--original-display-transformers-list by 'cddr do
           (ivy-rich-restore-transformer cmd))
  (setq ivy-rich--original-display-transformers-list nil))

(defun ivy-rich-setup-project-root-cache-mode ()
  (add-hook 'kill-buffer-hook 'ivy-rich-project-root-cache-kill-buffer-hook))

(defun ivy-rich-cleanup-project-root-cache-mode ()
  (ivy-rich-clear-project-root-cache)
  (remove-hook 'kill-buffer-hook 'ivy-rich-project-root-cache-kill-buffer-hook))

;;;###autoload
(define-minor-mode ivy-rich-mode
  "Toggle ivy-rich mode globally."
  :global t
  (if ivy-rich-mode
      (unless ivy-rich--original-display-transformers-list
        (ivy-rich-set-display-transformer 'backup))
    (ivy-rich-unset-display-transformer)))

;;;###autoload
(defun ivy-rich-reload ()
  (when ivy-rich-mode
    (ivy-rich-mode -1)
    (ivy-rich-mode 1)))

;;;###autoload
(define-minor-mode ivy-rich-project-root-cache-mode
  "Toggle ivy-rich-root-cache-mode globally."
  :global t
  (if ivy-rich-project-root-cache-mode
      (ivy-rich-setup-project-root-cache-mode)
    (ivy-rich-cleanup-project-root-cache-mode)))

(provide 'ivy-rich)

;;; ivy-rich.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
