;;; remark-mode.el --- Major mode for the remark slideshow tool -*- lexical-binding: t -*-

;; Copyright (C) 2015 Torgeir Thoresen

;; Author: @torgeir
;; Version: 1.8.0
;; Keywords: remark, slideshow, markdown, hot reload
;; Package-Requires: ((emacs "25.1") (markdown-mode "2.0"))

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

(require 'seq)
(require 'markdown-mode)

(defvar remark-preferred-browser
  "Google Chrome"
  "The applescript name of the application that the user's default browser.")

(defvar remark--folder
  (file-name-directory (locate-file "remark-mode.el" load-path))
  "Folder containing default remark skeleton file remark.html.")

(defvar remark--last-cursor-pos 0
  "The last recorded position in a .remark buffer.")

(defvar remark--last-move-timer nil
  "The last queued timer to visit the slide after cursor move.")

(defconst remark--is-osx (equal system-type 'darwin)
  "Is ‘remark-mode’ running on os x.")

(defun remark--file-as-string (file-path)
  "Get file contents from file at FILE-PATH as string."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))

(defun remark-next-slide ()
  "Skip to next slide."
  (interactive)
  (end-of-line)
  (if (search-forward-regexp "^--" nil t)
      (move-beginning-of-line 1)
    (end-of-buffer)))

(defun remark-prev-slide ()
  "Skip to prev slide."
  (interactive)
  (if (search-backward-regexp "^--" nil t)
      (move-beginning-of-line 1)
    (beginning-of-buffer)))

(defun remark-new-separator (sep)
  "Add separator SEP at end of next slide."
  (remark-next-slide)
  (if (= (point) (point-max))
      (insert (concat "\n" sep "\n"))
    (progn
      (insert (concat sep "\n\n"))
      (previous-line))))

(defun remark-new-slide ()
  "Create new slide."
  (interactive)
  (remark-new-separator "---")
  (save-buffer))

(defun remark-create-note ()
  "Create note for slide."
  (interactive)
  (remark-new-separator "???")
  (save-buffer))

(defun remark-new-incremental-slide ()
  "Create new incremental slide."
  (interactive)
  (remark-new-separator "--")
  (save-buffer))

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
      (move-beginning-of-line nil))
    (save-buffer)))

(defun remark--output-file-name ()
  "Optional user provided index.html file to write html slide set back to."
  (concat (file-name-directory (buffer-file-name)) "index.html"))

(defun remark--write-output-file (template-file content out-file)
  "Weave TEMPLATE-FILE together with CONTENT to create slide show. Write the result to OUT-FILE."
  (when-let (template-file-content (remark--file-as-string template-file))
    (let* ((positions (with-temp-buffer
                        (insert template-file-content)
                        (cons
                         (progn (beginning-of-buffer)
                                (search-forward "<textarea id=\"source\">")
                                (- (point) 1))
                         (progn (end-of-buffer)
                                (search-backward "</textarea>")
                                (- (point) 1)))))
           (textarea-start (car positions))
           (textarea-end (cdr positions)))
      (let ((out-file-content (concat (substring template-file-content 0 textarea-start)
                                      content
                                      (substring template-file-content textarea-end (length template-file-content)))))
        (write-region out-file-content nil (or out-file template-file) nil)))))

(defun remark--write-output-files ()
  "Write the remark output index.html file to the same folder as the .remark file for the resulting slide show."
  (let* ((default-remark-template (concat remark--folder "remark.html"))
         (user-out-file (file-truename (remark--output-file-name)))
         (markdown (buffer-string)))
    (remark--write-output-file (if (file-exists-p user-out-file)
                                   user-out-file
                                 default-remark-template) markdown user-out-file)))

(defun remark--run-osascript (s)
  "Run applescript S."
  (replace-regexp-in-string
   "[\r\n]+"
   ""
   (shell-command-to-string
    (format "osascript -e '%s'" s))))

(defun remark--osascript-get-frontmost-url ()
  (remark--run-osascript
   (format "tell application \"%s\" to get URL of active tab of first window"
           remark-preferred-browser)))

(defun remark--is-frontmost-url-remark ()
  (string-prefix-p "http://localhost:3000/#" (remark--osascript-get-frontmost-url)))

(defun remark--osascript-show-slide (n)
  "Run applescript to make browser navigate to slide N."
  (let* ((url (remark--osascript-get-frontmost-url))
         (slide (cadr (split-string url "#")))
         (presenter-mode (replace-regexp-in-string "[0-9]+" "" slide))
         (next-slide-url (concat "http://localhost:3000/#" presenter-mode (number-to-string n))))
    (remark--run-osascript
     (format "tell application \"%s\" to set URL of active tab of window 1 to \"%s\""
             remark-preferred-browser
             next-slide-url))))

(defun remark--is-connected ()
  "Check if ‘remark-mode’ is connected to browser sync."
  (get-buffer "*remark browser-sync*"))

(defun remark-visit-slide-in-browser ()
  "Visit slide at point in browser."
  (interactive)
  (when (and (remark--is-connected)
             (remark--is-frontmost-url-remark))
    (let* ((lines (split-string (buffer-substring (point-min) (point)) "\n"))
           (slide-lines (seq-filter (lambda (line)
                                      (or (string-prefix-p "layout: true" line)
                                          (string-prefix-p "--" line)))
                                    lines)))
      (remark--osascript-show-slide
       (max 1 (seq-reduce #'+ (seq-map (lambda (line)
                                         (if (string-prefix-p "layout: true" line) -1 1))
                                       slide-lines) 1))))))

(defun remark--post-command ()
  "Post command hook that queues a slide visit after some amount of time has occurred."
  (when (and (remark--is-connected)
             (string-suffix-p ".remark" buffer-file-name))
    (when remark--last-move-timer
      (cancel-timer remark--last-move-timer))
    (setq remark--last-move-timer
          (run-at-time "0.4 sec" nil (lambda ()
                                       (remark--write-output-files)
                                       (unless (equal (point) remark--last-cursor-pos)
                                         (remark-visit-slide-in-browser))
                                       (setq remark--last-cursor-pos (point)
                                             remark--last-move-timer nil))))))

(defun remark-connect-browser ()
  "Serve folder with browsersync."
  (interactive)
  (remark--write-output-files)
  (async-shell-command
   (concat "browser-sync start --server "
           (shell-quote-argument (file-truename (file-name-directory (remark--output-file-name))))
           " --no-open --no-ui --no-online")
   "*remark browser-sync*"
   "*remark browser-sync error*")
  (sit-for 1)
  (message "remark browser-sync connected")
  (browse-url "http://localhost:3000"))

(defun remark--save-hook ()
  "Hook to reload ‘remark-mode’ buffers when saved."
  (when (string-suffix-p ".remark" buffer-file-name)
    (save-buffer)
    (if (remark--is-connected)
        (progn
          (remark--write-output-files)
          (unless remark--is-osx
            (shell-command "browser-sync reload")))
      (concat "Wrote " buffer-file-name ". "
              "Use C-c C-s c to connect to a browser using browser-sync!"))))

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
   (cons "\\(background-image\\|class\\|count\\|layout\\|name\\|template\\)" font-lock-comment-face))
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
    (add-hook 'after-save-hook 'remark--save-hook)
    (when remark--is-osx
      (make-variable-buffer-local 'remark--last-cursor-por)
      (make-variable-buffer-local 'remark--last-move-timer)
      (add-hook 'post-command-hook 'remark--post-command))))

(provide 'remark-mode)
;;; remark-mode.el ends here
