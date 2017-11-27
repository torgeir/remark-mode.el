;;; remark-mode.el --- Major mode for the remark slideshow tool

;; Copyright (C) 2015 Torgeir Thoresen

;; Author: @torgeir
;; Version: 1.2.4
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

(defconst remark--is-osx (equal system-type 'darwin))

(defvar remark-preferred-browser
  "Google Chrome"
  "The applescript name of the application that the user's default browser.")

(defun remark-util-is-point-at-end-of-buffer ()
  "Check if point is at end of file."
  (= (point) (point-max)))

(defun remark-util-replace-string (old new s)
  "Replace OLD with NEW in S."
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun remark-util-file-as-string (file-path)
  "Get file contents from file at FILE-PATH as string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun remark-next-slide ()
  "Skip to next slide."
  (interactive)
  (end-of-line)
  (if (search-forward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (end-of-buffer))
  (when remark--is-osx (remark-visit-slide-in-browser)))

(defun remark-prev-slide ()
  "Skip to prev slide."
  (interactive)
  (if (search-backward-regexp "---" nil t)
      (move-beginning-of-line 1)
    (beginning-of-buffer))
  (when remark--is-osx (remark-visit-slide-in-browser)))

(defun remark-new-separator (sep)
  "Add separator SEP at end of next slide."
  (remark-next-slide)
  (if (remark-util-is-point-at-end-of-buffer)
      (insert (concat "\n" sep "\n"))
    (progn
      (insert (concat sep "\n\n"))
      (previous-line))))

(defun remark-new-slide ()
  "Create new slide."
  (interactive)
  (remark-new-separator "---"))

(defun remark-create-note ()
  "Create note for slide."
  (interactive)
  (remark-new-separator "???"))

(defun remark-new-incremental-slide ()
  "Create new incremental slide."
  (interactive)
  (remark-new-separator "--"))

(defun remark-kill-slide ()
  "Kill the current slide."
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
  "Folder containing remark skeleton file remark.html."
  :type 'string
  :group 'remark)

(defun remark-reload-in-browser ()
  "Preview slideshow in browser."
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

(defun remark--run-osascript (s)
  "Run applescript S."
  (shell-command (format "osascript -e '%s'" s)))

(defun remark--osascript-show-slide (n)
  "Run applescript to make browser navigate to slide N."
  (remark--run-osascript
   (format "tell application \"%s\" to set URL of active tab of window 1 to \"http://localhost:3000/#p%s\""
           remark-preferred-browser
           n)))

(defun remark-visit-slide-in-browser ()
  "Visit slide at point in browser."
  (interactive)
  (let ((slides (split-string
                 (buffer-substring (point-min)
                                   (min (point-max) (+ (point) 3)))
                 "---")))
    (remark--osascript-show-slide (length slides))))

(defun remark-connect-browser ()
  "Serve folder with browsersync."
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
  "Save the file and reloads in browser."
  (interactive)
  (save-buffer)
  (if (get-buffer "*remark browser-sync*")
      (remark-reload-in-browser)
    (message
     (concat "Wrote " buffer-file-name ". "
             "Use C-c C-s c to connect to a browser using browser-sync!"))))

(defun remark-save-hook ()
  "Hook to reload ‘remark-mode’ buffers when saved."
  (when (string-suffix-p ".remark" buffer-file-name)
    (remark-save)))

(defvar remark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'remark-next-slide)
    (define-key map (kbd "M-p") 'remark-prev-slide)
    (define-key map (kbd "C-c C-s s") 'remark-new-slide)
    (define-key map (kbd "C-c C-s i") 'remark-new-incremental-slide)
    (define-key map (kbd "C-c C-s k") 'remark-kill-slide)
    (define-key map (kbd "C-c C-s n") 'remark-create-note)
    (define-key map (kbd "C-c C-s c") 'remark-connect-browser)
    map)
  "Keymap for `remark-mode'.")

(defvar remark-mode-syntax-table
  (let ((st (make-syntax-table))) st)
  "Syntax table for `remark-mode'.")

(defconst remark-font-lock-defaults
  (list
   (cons "---" font-lock-warning-face)
   (cons "\\?\\?\\?" font-lock-comment-face)
   (cons "\\(template\\|name\\|class\\)" font-lock-comment-face))
  "Keyword highlight for `remark-mode'.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.remark\\'" . remark-mode))

;;;###autoload
(define-derived-mode
  remark-mode
  markdown-mode
  "remark"
  "A major mode for editing remark files."
  :syntax-table remark-mode-syntax-table
  (progn
    (setq font-lock-defaults
          (list (append
                 remark-font-lock-defaults
                 markdown-mode-font-lock-keywords-math
                 markdown-mode-font-lock-keywords-basic)))
    (add-hook 'after-save-hook 'remark-save-hook)))

(provide 'remark-mode)
;;; remark-mode.el ends here
