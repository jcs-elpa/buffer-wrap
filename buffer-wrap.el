;;; buffer-wrap.el --- Wrap the beginning and the end of buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-02-22 16:13:15

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Wrap the beginning and the end of buffer.
;; Keyword: buffer tool wrap
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/jcs090218/buffer-wrap

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Wrap the beginning and the end of buffer.
;;

;;; Code:


(defgroup buffer-wrap nil
  "Wrap the beginning and the end of buffer."
  :prefix "buffer-wrap-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/buffer-wrap"))


(defcustom buffer-wrap-line-changed-hook nil
  "Hooks run every time the line has changed."
  :type 'hook
  :group 'buffer-wrap)

(defvar-local buffer-wrap--relative-min-line 0
  "Relative line counting from the first line to wrap the buffer.
The default value is 0.")

(defvar-local buffer-wrap--relative-max-line -1
  "Relative line counting from the last line to wrap the buffer.
The default value is -1.")


;;;###autoload
(define-minor-mode buffer-wrap-mode
  "Minor mode 'buffer-wrap-mode'."
  :lighter " BW"
  :group 'buffer-wrap
  (if buffer-wrap-mode
      (buffer-wrap--enable)
    (buffer-wrap--disable)))


(defun buffer-wrap--goto-line (ln)
  "Goto LN line number."
  (goto-char (point-min))
  (forward-line (1- ln))
  (run-hooks 'buffer-wrap-line-changed-hook))

(defun buffer-wrap--around-line-move (fnc &rest args)
  "Post command for `buffer-wrap' with FNC and ARGS."
  (if (not buffer-wrap-mode)
      (apply fnc args)
    (let ((delta-ln (if (listp args) (nth 0 args) args))
          (current-ln (line-number-at-pos))
          (min-ln (+ (line-number-at-pos (point-min)) buffer-wrap--relative-min-line))
          (max-ln (+ (line-number-at-pos (point-max)) buffer-wrap--relative-max-line)))
      (ignore-errors (apply fnc args))
      (cond ((and (>= min-ln current-ln) (> 0 delta-ln))
             (buffer-wrap--goto-line max-ln))
            ((and (<= max-ln current-ln) (< 0 delta-ln))
             (buffer-wrap--goto-line min-ln))))))

(defun buffer-wrap--post-command ()
  "Post command for `buffer-wrap'."
  (let ((current-ln (line-number-at-pos))
        (min-ln (+ (line-number-at-pos (point-min)) buffer-wrap--relative-min-line))
        (max-ln (+ (line-number-at-pos (point-max)) buffer-wrap--relative-max-line)))
    (cond ((< current-ln min-ln)
           (buffer-wrap--goto-line min-ln))
          ((> current-ln max-ln)
           (buffer-wrap--goto-line max-ln)))))

(defun buffer-wrap--enable ()
  "Enable 'buffer-wrap-mode."
  (add-hook 'post-command-hook #'buffer-wrap--post-command nil t)
  (advice-add 'line-move :around #'buffer-wrap--around-line-move))

(defun buffer-wrap--disable ()
  "Disable 'buffer-wrap-mode."
  (remove-hook 'post-command-hook #'buffer-wrap--post-command t)
  (advice-remove 'line-move #'buffer-wrap--around-line-move))


(provide 'buffer-wrap)
;;; buffer-wrap.el ends here
