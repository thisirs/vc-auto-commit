;;; vc-git-commit-all.el --- Auto-committing tool for Git

;; Copyright (C) 2012 Sylvain Rousseau <thisirs at gmail dot com>

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords: vc, convenience, git

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

;; This package allows you to automatically commit all the changes of
;; a repository. It is useful when you have a project that needs to be
;; put under a version control system but you don't want to write
;; any commit message.
;; This package is written on top of emacs `vc-git' package.

;;; Code

(defvar vc-git-auto-commit-list
  nil
  "Lists auto-committed repo.")

(defvar vc-git-auto-committed-repo nil
  "Buffer local variable that controls whether corresponding file
  is in a auto-committed repository.")
(make-variable-buffer-local 'vc-git-auto-committed-repo)

(defvar vc-git-commit-msg-function 'vc-git-commit-msg
  "Function that returns a commit message.")

(defun vc-git-commit-msg ()
  "Return default commit message."
  (with-temp-buffer
    (insert (current-time-string) "\n")
    (vc-git-command t t nil
                    "diff-index" "--name-status"
                    "HEAD")
    (buffer-string)))

(defun vc-git-commit-all (&optional arg)
  "Commit all changes of `default-directory' repository. If used
with \\[prefix-argument], the user is asked for a commit message.
Otherwise, `vc-git-commit-msg-function' is called to generate a
commit message."
  (interactive "P")
  (with-temp-buffer
    (vc-git-command t 0 nil "status" "--porcelain")
    (if (zerop (buffer-size (current-buffer)))
        (message "Nothing to commit in repo %s" default-directory)
      (message "Auto-committing repo %s..." default-directory)
      (vc-git-command nil 0 nil "add" "-A" ".")
      (vc-git-command nil 0 nil "commit" "-m"
                      (if arg (read-string "Commit message: ")
                        (funcall vc-git-commit-msg-function)))
      (message "Auto-committing repo %s...done" default-directory))))

(defun vc-git-auto-committed-p (&optional file)
  "Returns t if file FILE is in an auto-committed repository."
  (setq file (or file (buffer-file-name)))
  (and (vc-git-root file)
       (vc-git-auto-committed-repo-p (vc-git-root file))))

(defun vc-git-auto-committed-repo-p (&optional root-dir)
  "Tests if ROOT-DIR is the root directory of an auto-committed
git repository."
  (setq root-dir (or root-dir default-directory))
  (and root-dir
       (or
        (file-exists-p
         (expand-file-name ".auto-commit" root-dir))
        (file-exists-p
         (expand-file-name ".autocommit" root-dir))
        (with-temp-buffer
          (and
           (let ((default-directory root-dir))
             (zerop (vc-git-command t 1 nil
                                    "config" "--bool" "--get" "core.autocommit")))
           (string-match "true" (buffer-string)))))))

(defun vc-git-list-all-auto-committed-repos ()
  "Returns a list of all auto-committed repositories by looking
at all opened files and at `vc-git-auto-commit-list'."
  (let (repo-list)
    ;; lists all auto-committed repos that has a file opened in emacs
    (dolist (buffer (buffer-list) repo-list)
      (let* ((file (buffer-file-name buffer))
             (git-root (and file (vc-git-root file))))
        (if (or (and (local-variable-p 'vc-git-auto-committed-repo)
                     vc-git-auto-committed-repo)
                (and git-root
                     (vc-git-auto-committed-repo-p git-root)))
            (add-to-list 'repo-list git-root))))
    ;; add repos from `vc-git-auto-commit-list'
    (dolist (repo vc-git-auto-commit-list repo-list)
      (add-to-list repo-list (file-name-as-directory repo)))))

(defun vc-git-auto-commit-repos ()
  "Stage and commit all changes of all known repos. A known repo
is either a repo whose root is in `vc-git-auto-commit-list' or a
repo with a file .autocommit or .auto-commit in its root or a
repo with core.autocommit git setting set to true."
  (let ((repos (vc-git-list-all-auto-committed-repos))
        repo)
    (while (and
            (setq repo (pop repos))
            (condition-case err
                (let ((default-directory repo))
                  (vc-git-commit-all) t)
              (error (yes-or-no-p
                      (format "An error occurred on repo %s: %s; Exit anyway?"
                              repo err))))))
    ;; return nil if repo not nil ie an error occurred and answer is no
    (not repo)))

(global-set-key (kbd "C-x v C") 'vc-git-commit-all)

(add-to-list 'kill-emacs-query-functions 'vc-git-auto-commit-repos)

(provide 'vc-git-commit-all)

;;; vc-git-commit-all.el ends here
