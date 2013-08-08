;;; vc-auto-commit-status-testrunner.el ---

;; Copyright (C) 2013 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; URL: http://github.com/thisirs/vc-auto-commit.git
;; Keywords:

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

;;; Commentary

;;; Code

(require 'ert)

(defadvice ert--pp-with-indentation-and-newline (around fix-display activate)
  "Do not truncate the results of a failing test."
  (let ((print-escape-newlines t)
        (print-level nil)
        (print-length nil))
    ad-do-it))

(let* ((current-directory (file-name-directory load-file-name))
       (test-path (expand-file-name "." current-directory))
       (root-path (expand-file-name ".." current-directory)))

  (add-to-list 'load-path root-path)

  ;; Activating vc-auto-commit
  (load (expand-file-name "vc-auto-commit.el" root-path) t t)

  ;; Loading the tests
  (load (expand-file-name "vc-git-auto-commit-test.el" test-path) t t)

  (ert-run-tests-batch-and-exit t))

;;; vc-auto-commit-testrunner.el ends here
