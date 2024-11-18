;;; list-unicode-display.el --- Search for and list unicode characters by name

;; Copyright (C) 2015  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: convenience
;; Package-Version: 0
;; Package-Requires: ((emacs "24.3"))

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

;; This is a packaged version of code by @jpkotta, taken from a
;; comment on http://tromey.com/blog/?p=831.

;;; Code:

(defgroup list-unicode-display nil
  "Explore unicode characters."
  :group 'i18n)

(define-derived-mode list-unicode-display-mode help-mode "Unicode Characters"
  "Major mode to display a list of unicode characters.")

(defface list-unicode-display-code-point
  '((t :foreground "light green"
       :weight bold))
  "Face for unicode code points."
  :group 'list-unicode-display)

(defun list-unicode-display-describe ()
  "Apply `describe-char' to the character in a row of a `list-unicode-display-mode' buffer."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position))
    (describe-char (point))))

(defun list-unicode-display-copy ()
  "Copy the character in a row of a `list-unicode-display-mode' buffer to the kill ring."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (search-forward "\t" (line-end-position))
    (kill-ring-save (point) (1+ (point)))
    (message "Saved `%s' to the kill-ring."
             (buffer-substring-no-properties (point) (1+ (point))))))

(defun list-unicode-display--hashtables ()
  "Return a cons cell of hastables for working with unicode data.

The car is the output of the function `ucs-names', and the cdr is the a
hashtable mapping unicode names to char values."
  ;; alist like ("name" . code-point)
  (let* ((char-alist ()))
    (let ((names (ucs-names)))
      (if (hash-table-p names)
          ;; ucs-names returns a hash table in emacs 26+
          (maphash (lambda (name char)
                     (push (cons name char) char-alist))
                   names)
        (mapc (lambda (pair)
                (push pair char-alist))
              names))
      (cons names char-alist))))

;;;###autoload
(define-key list-unicode-display-mode-map (kbd "RET") #'list-unicode-display-describe)
(define-key list-unicode-display-mode-map (kbd "w") #'list-unicode-display-copy)
(define-key list-unicode-display-mode-map (kbd "g") #'list-unicode-display)

;;;###autoload
(defun list-unicode-display (&optional regexp)
  "Display a list of unicode characters with names matching REGEXP.
If no regexp is supplied, all characters are shown.  This takes
some time."
  (interactive "sRegexp (default \".*\"): ")
  (pcase-let* ((`(,names . ,char-alist) (list-unicode-display--hashtables))
               (regexp (or regexp ".*"))
               (case-fold-search t)
               (cmp (lambda (x y) (< (cdr x) (cdr y))))
               (pred (lambda (name) (string-match-p regexp name))))

    (setq char-alist (sort char-alist cmp))

    (let ((buf (get-buffer-create "*Unicode Characters*"))
          (display-buffer-base-action '(display-buffer-same-window . nil)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (c char-alist)
            (insert (format "0x%06X\t" (cdr c)))
            (insert (char-to-string (cdr c)))
            (insert (format "\t%s\n" (car c))))
          (list-unicode-display-mode)
          (goto-char (point-min))))
      (display-buffer buf))))


;;;###autoload
(defun list-unicode-display-find-copy ()
  "Copy a prompted character to the kill ring."
  (interactive)
  (pcase-let ((`(,names . ,char-alist) (list-unicode-display--hashtables)))
    (let ((completion-extra-properties
           `(:affixation-function
             ,(lambda (completions)
                (let ((max-name-width (if (null completions)
                                          0
                                        (seq-max (mapcar #'length completions)))))
                  (mapcar (lambda (name)
                            (let* ((char (alist-get name char-alist nil nil #'string=))
                                   (char-hex (format "%03X" char))
                                   (unicode-point (concat "U+" char-hex)))
                              (list name
                                    ""
                                    (format "%s  %s %s %s"
                                            (make-string (- max-name-width (length name)) ?\s)
                                            (propertize unicode-point 'face 'list-unicode-display-code-point)
                                            ;; Max unicode code point is U+10FFFF
                                            (make-string (- 6 (length char-hex)) ?\s)
                                            (char-to-string char)))))
                          completions))))))
      (kill-new (char-to-string
                 (alist-get (completing-read "Find: " names) char-alist nil nil #'string=))))))

(provide 'list-unicode-display)
;;; list-unicode-display.el ends here
