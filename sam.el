;;; sam.el --- sam for Emacs.  -*- lexical-binding: t -*-

;; Copyright © 2020 Omar Polo <op@omarpolo.com>

;; This file is not part of GNU Emacs.

;; This file is free software.
;;
;; Permission to use, copy, modify, and distribute this software for
;; any purpose with or without fee is hereby granted, provided that
;; the above copyright notice and this permission notice appear in all
;; copies.
;;
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;; Author: Omar Polo <op@omarpolo.com>
;; URL: https://git.omarpolo.com/sam.el
;; Keywords: emulation

;;; Commentary

;; TODO.

;;; Code:

(eval-when-compile                      ; subr-x.el says so.
  (require 'subr-x))

(require 'cl-lib)

(defgroup sam nil
  "sam for Emacs."
  :prefix "sam-"
  :group 'emulations
  :link '(url-link :tag "Git repo" "https://git.omarpolo.com/sam.el"))

(defvar sam-current-buffer nil
  "The active buffer where command would operate on.'")

(defvar sam-prompt nil
  "The sam prompt.  If nil, no prompt will be printed")

(defvar sam-is-inserting nil
  "t if sam is accepting text input instead of commands.")

(defvar sam-last-replace nil
  "last replaced regexp.")

(defvar sam-mark-alist nil
  "Sam marks.")

(defvar sam-mode-hook nil
  "Hook run when entering sam mode.")

(defvar sam-mode-map (make-sparse-keymap)
  "Keymap for sam mode.")

(define-key sam-mode-map "\r" 'sam-newline)

(defconst sam-cmd-alist
  '(("=#" . sam-cmd-charoffset)         ; before = so it can get matched
    ("="  . sam-cmd-linenum)
    ("P"  . sam-cmd-filename)
    ("p"  . sam-cmd-print)
    ("b"  . sam-cmd-switch-buffer)
    ("B"  . sam-cmd-switch-buffer-no-fuzzy)
    ("n"  . sam-cmd-buflist)
    ("q"  . sam-cmd-quit)))

(defun sam-get-buffer ()
  "Gets the buffer of this sam instance."
  (get-buffer "*sam*"))

(defun sam-report-error (err)
  (with-current-buffer (sam-get-buffer)
    (insert "?" err)))

(defun sam-parse-line (s)
  "Returns an cons of `address' and `command'"
  (let ((address "")
        (command "")
        (run t)
        (i 0)
        (len (length s)))
    (while (and run
                (< i len))
      (if (cl-digit-char-p (aref s i))
          (setq i (1+ i))
        (setq run nil)))
    (setq address (substring s 0 i)
          command (substring s i))
    `(,address . ,(string-trim-left command))))

(defun sam-dot-select-line (lineno)
  (with-current-buffer sam-current-buffer
    (with-no-warnings
      (goto-line lineno)
      (let (p1 p2)
        (setq p1 (line-beginning-position)
              p2 (line-end-position))
        (goto-char p2)
        (push-mark p1)
        (setq mark-active t)))))

(defun sam-set-dot (address)
  (when sam-current-buffer
    (if (string-equal address "")
        nil
      (sam-dot-select-line (string-to-number address)))))

(defun sam-get-line ()
  "Returns the string on the current line."
  (copy-region-as-kill
   (+ (if (and (not sam-is-inserting)
               sam-prompt)
          (length sam-prompt)
        0)
      (point-at-bol))
   (point-at-eol))
  (pop kill-ring))

(defun sam-parse-command (cmd)
  (cl-loop
   for (command . fn) in sam-cmd-alist
   when (string-equal command cmd) return `(,command . ,fn)))

(defun sam-exec-command (cmd)
  (let ((tmp (sam-parse-command cmd)))
    (if tmp
        (cl-destructuring-bind (cmd . fn) tmp
          (funcall fn (substring cmd (length cmd))))
      (sam-report-error (concat "unknown command: " cmd)))))

(defun sam-exec-line ()
  "Run the sam command on this line."
  (let* ((line   (sam-get-line))
         (parsed (sam-parse-line line))
         (addr   (car parsed))
         (cmd    (cdr parsed)))
    (insert "\n")
    (goto-char (point-max))
    (sam-set-dot addr)
    (sam-exec-command cmd)))

(defun sam-newline ()
  "Insert a newline, executing the command on this line if in
  command mode."
  (interactive)
  (sam-exec-line)
  (message "foo")
  (if (and (not sam-is-inserting) sam-prompt)
      (insert sam-prompt)))

(defun sam-current-buffer-p (buf)
  "True if `buf' is the current sam buffer."
  (string-equal (buffer-name buf)
                (buffer-name sam-current-buffer)))

(defun sam-current-buffer-region ()
  "Returns a cons of `region-beginning' . `region-end' in the current buffer."
  (with-current-buffer sam-current-buffer
    `(,(region-beginning) . ,(region-end))))

(defun sam-list-file-buffers ()
  "Return a list of buffer that are visiting a file."
  (cl-loop for buf in (buffer-list)
           when (buffer-file-name buf)
           collect buf))

(defun sam-get-region-as-string ()
  (with-current-buffer sam-current-buffer
    (buffer-substring-no-properties (region-beginning)
                                    (region-end))))

(defun sam-cmd-charoffset (_arg)
  (cl-destructuring-bind (begin . end) (sam-current-buffer-region)
    (with-current-buffer (sam-get-buffer)
      (if (= begin end)
          (insert "#" (number-to-string begin) "\n")
        (insert "#" (number-to-string begin) ","
                "#" (number-to-string end)   "\n")))))

(defun sam-cmd-linenum (_arg)
  (cl-destructuring-bind (begin . end) (sam-current-buffer-region)
    (with-current-buffer (sam-get-buffer)
      (if (= begin end)
          (insert (number-to-string (line-number-at-pos begin)) "; "
                  "#" (number-to-string begin) "\n")
        (insert (number-to-string (line-number-at-pos begin)) ","
                (number-to-string (line-number-at-pos end)) "; "
                "#" (number-to-string begin) ","
                "#" (number-to-string end)   "\n")))))

(defun sam-cmd-filename (_arg)
  (with-current-buffer (sam-get-buffer)
    (insert (buffer-name sam-current-buffer) "\n")))

(defun sam-cmd-print (_arg)
  (let ((s (sam-get-region-as-string)))
    (with-current-buffer (sam-get-buffer)
      (insert s "\n"))))

(defun sam-cmd-switch-buffer (_arg)
  (with-current-buffer (sam-get-buffer)
    (sam-report-error "not implemented yet")))

(defun sam-cmd-switch-buffer-no-fuzzy (_arg)
  (with-current-buffer (sam-get-buffer)
    (sam-report-error "not implemented yet")))

(defun sam-cmd-buflist (_arg)
  (with-current-buffer (sam-get-buffer)
    (dolist (buf (sam-list-file-buffers))
      (insert
       (if (buffer-modified-p buf) "'" " ")
       ;; visible or not visible buffer? -, + or * for multiple window
       (if (sam-current-buffer-p buf) "." " ")
       " "
       (buffer-name buf)
       "\n"))))

(defun sam-cmd-quit (_arg)
  (kill-buffer (sam-get-buffer)))

(defun sam-mode ()
  "Major mode for sam buffers."
  (kill-all-local-variables)
  (use-local-map sam-mode-map)
  (setq mode-name "sam")
  (setq major-mode 'sam-mode)
  (run-hooks 'sam-mode-hook))

(defun sam ()
  "Launch a sam session associated with the current buffer."
  (interactive)
  (let ((edit-buffer (buffer-name)))
    (pop-to-buffer "*sam*")
    (sam-mode)
    (setq sam-current-buffer (get-buffer edit-buffer))))

(provide 'sam)
