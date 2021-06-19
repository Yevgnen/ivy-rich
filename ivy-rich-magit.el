;;; ivy-rich-magit.el --- ivy-rich integration with magit -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Yevgnen Koh

;; Author: Egor Duplensky <egor.duplensky@gmail.com>

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

;; Display additional information, such as remote branch, commit sha,
;; tag, commit subject when selecting magit revisions
;; Usage:
;; (require 'ivy-rich-magit)
;; (ivy-rich-magit-mode 1)
;;
;; See documentation on https://github.com/yevgnen/ivy-rich.

;;; Code:

(require 'ivy-rich)
(require 'magit)

(setq ivy-rich-magit-display-transformers-list
      '(magit-checkout
        (:columns
         ((magit-get-branch-and-upstream (:width 0.35))
          (magit-get-current-commit-sha-short (:width 11 :face magit-hash))
          (magit-get-current-tag-save (:width 10 :face magit-tag))
          (magit-commit-message (:width 0.35 :face font-lock-comment-face))
          ))
        magit-branch-and-checkout
        ivy-rich--magit-checkout-transformer
        magit-branch-create
        ivy-rich--magit-checkout-transformer
        magit-branch-configure
        ivy-rich--magit-checkout-transformer
        magit-branch-rename
        ivy-rich--magit-checkout-transformer
        magit-branch-delete
        ivy-rich--magit-checkout-transformer
        magit-branch-reset
        ivy-rich--magit-checkout-transformer
        magit-cherry-apply
        ivy-rich--magit-checkout-transformer
        magit-bisect-start
        ivy-rich--magit-checkout-transformer
        magit-pull-from-upstream
        ivy-rich--magit-checkout-transformer
        magit-pull-from-pushremote
        ivy-rich--magit-checkout-transformer
        magit-pull-branch
        ivy-rich--magit-checkout-transformer
        magit-merge-plain
        ivy-rich--magit-checkout-transformer
        magit-merge-editmsg
        ivy-rich--magit-checkout-transformer
        magit-merge-nocommit
        ivy-rich--magit-checkout-transformer
        magit-merge-preview
        ivy-rich--magit-checkout-transformer
        magit-merge-squash
        ivy-rich--magit-checkout-transformer
        magit-merge-into
        ivy-rich--magit-checkout-transformer
        magit-push-current
        ivy-rich--magit-checkout-transformer
        magit-push-tag
        ivy-rich--magit-checkout-transformer
        magit-push-other
        ivy-rich--magit-checkout-transformer
        magit-push-refspecs
        ivy-rich--magit-checkout-transformer
        magit-rebase-branch
        ivy-rich--magit-checkout-transformer
        magit-tag-delete
        ivy-rich--magit-checkout-transformer
        magit-tag-create
        ivy-rich--magit-checkout-transformer
        magit-notes-edit
        ivy-rich--magit-checkout-transformer
        magit-notes-remove
        ivy-rich--magit-checkout-transformer
        magit-revert-and-commit
        ivy-rich--magit-checkout-transformer
        magit-revert-no-commit
        ivy-rich--magit-checkout-transformer
        magit-cherry
        ivy-rich--magit-checkout-transformer
        magit-worktree-checkout
        ivy-rich--magit-checkout-transformer
        ))

(defun magit-get-current-commit-sha-short (rev)
  (magit-rev-parse "--short" rev))

(defun magit-commit-message (rev)
  (magit-rev-format "%s" rev))

(defun magit-get-branch-and-upstream (rev)
  (let ((branch (magit-get-upstream-branch rev)))
    (concat (propertize rev 'face 'default) (when branch
                                              (put-text-property 0 (length branch) 'face 'magit-branch-remote-head branch)
                                              (format " %s" branch)))
    ))

(defun magit-get-current-tag-save (rev)
  (let ((tag (magit-git-str "describe" "--tags" "--exact-match" rev)))
    (if tag tag "")
    ))

(defun ivy-rich-setup-magit-mode ()
  ;; (setq ivy-rich-display-transformers-list-backup ivy-rich-display-transformers-list)
  (setq ivy-rich-display-transformers-list (append ivy-rich-display-transformers-list
                                                   ivy-rich-magit-display-transformers-list))
  (ivy-rich-set-display-transformer 'backup))

(defun ivy-rich-cleanup-magit-mode ()
  (setq ivy-rich-display-transformers-list
        (seq-difference ivy-rich-display-transformers-list ivy-rich-magit-display-transformers-list))
  (ivy-rich-unset-display-transformer)
  (ivy-rich-set-display-transformer nil))

;;;###autoload
(define-minor-mode ivy-rich-magit-mode
  "Toggle ivy-rich-magit-mode globally."
  :global t
  (if ivy-rich-magit-mode
      (ivy-rich-setup-magit-mode)
    (ivy-rich-cleanup-magit-mode)))

(provide 'ivy-rich-magit)

;;; ivy-rich-magit.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
