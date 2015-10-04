;;; remark-mode.el --- Major mode for the remark slideshow tool

;; Copyright (C) 2015 Torgeir Thoresen

;; Author: @torgeir
;; Version: 1.0.0
;; Keywords: remark, slideshow, markdown
;; Package-Requires: ((markdown-mode "2.0"))

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

;; A major mode for remark, the simple, in-browser, markdown-driven
;; slideshow tool
;;
;; https://github.com/gnab/remark

;;; Code:

(require 'markdown-mode)

(defun remark-util-is-point-at-end-of-buffer ()
  "checks if point is at end of file"
  (= (point) (point-max)))

(defun remark-util-replace-string (old new s)
  "replaces old with new in s"
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun remark-util-file-as-string (file-path)
  "get file contents as string"
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun remark-next-slide ()
  "skip to next slide"
  (interactive)
  (end-of-line)
  (if (search-forward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (end-of-buffer)))

(defun remark-prev-slide ()
  "skip to prev slide"
  (interactive)
  (if (search-backward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (beginning-of-buffer)))

(defun remark-new-separator (sep)
  "adds separator at end of next slide"
  (remark-next-slide)
  (if (remark-util-is-point-at-end-of-buffer)
      (insert (concat "\n" sep "\n"))
    (progn
      (insert (concat sep "\n\n"))
      (previous-line))))

(defun remark-new-slide ()
  "creates new slide"
  (interactive)
  (remark-new-separator "---"))

(defun remark-create-note ()
  "creates note for slide"
  (interactive)
  (remark-new-separator "???"))

(defun remark-new-incremental-slide ()
  "creates new incremental slide"
  (interactive)
  (remark-new-separator "--"))

(defun remark-kill-slide ()
  "kills the current slide"
  (interactive)
  (remark-prev-slide)
  (let ((current-slide-start (point)))
    (next-line)
    (let* ((has-next-slide-marker (search-forward-regexp "---" nil t))
           (next-slide-start (match-beginning 0)))
      (kill-region current-slide-start
                   (if has-next-slide-marker
                       next-slide-start
                     (point-max)))
      (move-beginning-of-line nil))))

(defcustom remark-folder
  (file-name-directory (locate-file "remark-mode.el" load-path))
  "folder containing remark skeleton file remark.html"
  :type 'string
  :group 'remark)

(defun remark-reload-in-browser ()
  "preview slideshow in browser"
  (interactive)
  (let* ((remark-file (concat remark-folder "remark.html"))
         (template-content (remark-util-file-as-string remark-file))
         (index-content (remark-util-replace-string
                         "</textarea>"
                         (concat (buffer-string) "</textarea>")
                         template-content))
         (index-file (concat remark-folder "index.html"))
         (index-file-nosymlink (file-truename index-file)))
    (write-region index-content nil index-file-nosymlink nil)
    (shell-command "browser-sync reload")))

(defun remark-connect-browser ()
  "serve folder with browsersync"
  (interactive)
  (async-shell-command
   (concat "browser-sync start --server "
           (shell-quote-argument (file-truename remark-folder))
           " --no-open --no-ui --no-online")
   "*remark browser-sync*"
   "*remark browser-sync error*")
  (sit-for 1)
  (message "remark browser-sync connected")
  (remark-save)
  (browse-url "http://localhost:3000"))

(defun remark-save ()
  "saves the file and reloads in browser"
  (interactive)
  (save-buffer)
  (if (get-buffer "*remark browser-sync*")
      (remark-reload-in-browser)
    (message
     (concat "Wrote " buffer-file-name ". "
             "Use C-c C-s c to connect to a browser using browser-sync!"))))

(defvar remark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'remark-next-slide)
    (define-key map (kbd "M-p") 'remark-prev-slide)
    (define-key map (kbd "C-x C-s") 'remark-save)
    (define-key map (kbd "C-c C-s s") 'remark-new-slide)
    (define-key map (kbd "C-c C-s i") 'remark-new-incremental-slide)
    (define-key map (kbd "C-c C-s k") 'remark-kill-slide)
    (define-key map (kbd "C-c C-s n") 'remark-create-note)
    (define-key map (kbd "C-c C-s c") 'remark-connect-browser)
    map)
  "keymap for `remark-mode'")

(defvar remark-mode-syntax-table
  (let ((st (make-syntax-table))) st)
  "syntax table for `remark-mode'")

(defconst remark-font-lock-defaults
  (list
   (cons "---" font-lock-warning-face)
   (cons "\\?\\?\\?" font-lock-comment-face)
   (cons "\\(template\\|name\\|class\\)" font-lock-comment-face))
  "keyword highlight for `remark-mode'")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.remark\\'" . remark-mode))

;;;###autoload
(define-derived-mode
  remark-mode
  markdown-mode
  "remark"
  "a major mode for editing remark files"
  :syntax-table remark-mode-syntax-table
  (setq font-lock-defaults (list (append remark-font-lock-defaults markdown-mode-font-lock-keywords))))

(provide 'remark-mode)
;;; remark-mode.el ends here
