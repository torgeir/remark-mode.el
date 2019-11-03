;;; remark-mode.el --- Major mode for the remark slideshow tool -*- lexical-binding: t -*-

;; Copyright (C) 2015 Torgeir Thoresen

;; Author: @torgeir
;; Version: 2.0.1
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


(defvar remark--folder
  (file-name-directory (locate-file "remark-mode.el" load-path))
  "Folder containing default remark skeleton file remark.html.")


(defvar remark--last-cursor-pos 1
  "The last recorded position in a .remark buffer.")


(defvar remark--last-move-timer nil
  "The last queued timer to visit the slide after cursor move.")


(defvar remark--is-presenter nil
  "Is presenter mode enabled in the slideset.")


(defun remark-toggle-presenter ()
  "Toggle remark presenter mode. Reloads the slideshow."
  (interactive)
  (setq remark--is-presenter (not remark--is-presenter))
  (remark--show-slide (remark--current-slide) t))


(defun remark--file-as-string (file-path)
  "Get file contents from file at FILE-PATH as string."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string))))


(defun remark-next-slide (&optional arg)
  "Skip to next slide.
Optional argument ARG skips to next incremental slide."
  (interactive "P")
  (end-of-line)
  (if (search-forward-regexp (if arg "^--" "^---") nil t)
      (move-beginning-of-line 1)
    (end-of-buffer)))


(defun remark-prev-slide (&optional arg)
  "Skip to prev slide.
Optional argument ARG skips to previous incremental slide."
  (interactive "P")
  (if (search-backward-regexp (if arg "^--" "^---") nil t)
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
    (let* ((has-next-slide-marker (search-forward-regexp "^---" nil t))
           (next-slide-start (match-beginning 0)))
      (kill-region current-slide-start
                   (if has-next-slide-marker
                       next-slide-start
                     (point-max)))
      (move-beginning-of-line nil))
    (when (and (= (point) (point-min))
               (looking-at "^---"))
      (delete-region (line-beginning-position) (1+ (line-end-position))))
    (save-buffer)))

(defun remark-move-slide-next ()
  "Move the slide past the next slide."
  (interactive)
  (when (not (or (remark--is-last-slide)
                 (save-excursion
                   (end-of-line)
                   (= (point-max) (point)))))
    (remark-kill-slide)
    (remark-next-slide)
    (let ((slide (with-temp-buffer
                   (yank)
                   (beginning-of-buffer)
                   (when (not (looking-at "^---"))
                     (insert "---\n"))
                   (buffer-string))))
      (insert slide))
    (remark-visit-slide-in-browser)))


(defun remark-move-slide-prev ()
  "Move the slide in front of the previous slide."
  (interactive)
  (remark-kill-slide)
  (remark-prev-slide)
  (if (= (point) (point-min))
      (let ((slide (with-temp-buffer
                     (yank)
                     (beginning-of-buffer)
                     (when (looking-at "^---")
                       (delete-region (line-beginning-position) (1+ (line-end-position))))
                     (buffer-string))))
        (insert slide)
        (insert "---\n")
        (previous-line))
    (yank))
  (when (not (looking-at "^"))
    (newline))
  (remark-visit-slide-in-browser))


(defun remark--is-last-slide ()
  "Check if the point is inside of the last slide."
  (interactive)
  (save-excursion
    (remark-prev-slide)
    (remark-next-slide)
    (= (point) (point-max))))


(defun remark--output-file-name ()
  "Optional user provided index.html file to write html slide set back to."
  (concat (file-name-directory (buffer-file-name)) "index.html"))


(defun remark--write-output-file (template-file content out-file)
  "Weave TEMPLATE-FILE together with CONTENT to create slideshow. Write the result to OUT-FILE."
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
  "Write the remark output index.html file to the same folder as the .remark file for the resulting slideshow."
  (let* ((default-remark-template (concat remark--folder "remark.html"))
         (user-out-file (file-truename (remark--output-file-name)))
         (markdown (buffer-string)))
    (remark--write-output-file (if (file-exists-p user-out-file)
                                   user-out-file
                                 default-remark-template) markdown user-out-file)))


(defun remark--close-tab ()
  "Close the slideshow browser window, if on os x."
  (if (and (eq system-type 'darwin)
           (string-match-p
            "http://localhost:3000"
            (shell-command-to-string
             "osascript -l JavaScript -e \"Application('Google Chrome').windows[0].activeTab().url()\"")))
      (shell-command-to-string
       "osascript -l JavaScript -e \"Application('Google Chrome').windows[0].activeTab().close()\"")))


(defun remark--current-slide ()
  "Fetch the current slide from the .slide file."
  (when-let ((slide (remark--file-as-string (remark--slide-file))))
    (string-to-number
     (replace-regexp-in-string
      "p" ""
      (replace-regexp-in-string
       "reload" ""
       (replace-regexp-in-string
        "\n+" ""
        slide))))))


(defun remark--slide-to-file (slide f)
  "Write SLIDE number to file F."
  (let ((cmd (concat "echo "
                     (shell-quote-argument (if (numberp slide)
                                               (number-to-string slide)
                                             slide))
                     " > "
                     (shell-quote-argument f))))
    (shell-command cmd)))


(defun remark--slide-file ()
  "Fetch path of the .slide file."
  (concat (file-name-directory (buffer-file-name)) ".slide"))


(defun remark--show-slide (n &optional reload)
  "Write slide number to file to make browser navigate to slide N.
Optional argument RELOAD reloads the slideshow in the browser."
  (when n
    (remark--slide-to-file (concat (if reload "reload" "")
                                   (or (and remark--is-presenter
                                            (concat "p" (number-to-string n)))
                                       (number-to-string n)))
                           (remark--slide-file))))


(defun remark--is-connected ()
  "Check if ‘remark-mode’ is connected to the browser."
  (get-buffer "*remark browser*"))


(defun remark-visit-slide-in-browser (&optional reload)
  "Visit slide at point in browser.
Optional argument RELOAD reloads the slideshow in the browser."
  (interactive)
  (when (remark--is-connected)
    (let* ((lines (split-string (buffer-substring (point-min) (point)) "\n"))
           (slide-lines
            (seq-filter (lambda (line)
                          (or (string-prefix-p "layout: true" line)
                              (string-prefix-p "--" line)))
                        lines)))
      (remark--show-slide
       (max 1 (seq-reduce #'+
                          (seq-map (lambda (line)
                                     (if (string-prefix-p "layout: true" line) -1 1))
                                   slide-lines)
                          1))
       reload))))


(defun remark--post-command ()
  "Post command hook that queues a slide visit after some amount of time has occurred."
  (when (and (remark--is-connected)
             (string-suffix-p ".remark" buffer-file-name))
    (when remark--last-move-timer
      (cancel-timer remark--last-move-timer))
    (setq remark--last-move-timer
          (run-at-time "0.4 sec" nil (lambda ()
                                       (when (and (remark--is-connected)
                                                  (string-suffix-p ".remark" buffer-file-name))
                                         (remark--write-output-files)
                                         (when (and
                                                (not (equal (point) remark--last-cursor-pos))
                                                (string-match-p
                                                 "^--"
                                                 (buffer-substring (min (point) (min (point-max) remark--last-cursor-pos))
                                                                   (max (point) (min (point-max) remark--last-cursor-pos)))))
                                           (remark-visit-slide-in-browser))
                                         (setq remark--last-cursor-pos (point)
                                               remark--last-move-timer nil)))))))


(defun remark-connect-browser ()
  "Connect the <slideshow>.remark file to the in browser remark slideshow."
  (interactive)
  (remark--write-output-files)
  (remark--show-slide (or (remark--current-slide) 1))
  (async-shell-command
   (concat "node "
           remark--folder
           "server.js "
           (file-name-directory buffer-file-name))
   "*remark browser*"
   "*remark browser error*")
  (message "remark browser connected")
  (browse-url "http://localhost:3000"))


(defun remark--save-hook ()
  "Hook to reload ‘remark-mode’ buffers when saved."
  (when (string-suffix-p ".remark" buffer-file-name)
    (if (remark--is-connected)
        (progn
          (remark--write-output-files)
          (remark-visit-slide-in-browser t))
      (concat "Wrote " buffer-file-name ". "
              "Use C-c C-s c to connect the slideshow to a browser! C-c C-s d to disconnect it."))))


(defun remark-kill-browser ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil)
        (kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                           kill-buffer-query-functions)))
    (kill-buffer "*remark browser*")
    (remark--close-tab)))


(defvar remark-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'remark-next-slide)
    (define-key map (kbd "M-p") 'remark-prev-slide)
    (define-key map (kbd "M-<down>") 'remark-next-slide)
    (define-key map (kbd "M-<up>") 'remark-prev-slide)
    (define-key map (kbd "M-S-<down>") 'remark-move-slide-next)
    (define-key map (kbd "M-S-<up>") 'remark-move-slide-prev)
    (define-key map (kbd "C-c C-s p") 'remark-toggle-presenter)
    (define-key map (kbd "C-c C-s s") 'remark-new-slide)
    (define-key map (kbd "C-c C-s i") 'remark-new-incremental-slide)
    (define-key map (kbd "C-c C-s k") 'remark-kill-slide)
    (define-key map (kbd "C-c C-s n") 'remark-create-note)
    (define-key map (kbd "C-c C-s c") 'remark-connect-browser)
    (define-key map (kbd "C-c C-s d") 'remark-kill-browser)
    map)
  "Keymap for `remark-mode'.")


(defvar remark-mode-syntax-table
  (let ((st (make-syntax-table))) st)
  "Syntax table for `remark-mode'.")


(defconst remark-font-lock-defaults
  (list
   (cons "--" font-lock-warning-face)
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
    (add-hook 'after-save-hook #'remark--save-hook)
    (make-variable-buffer-local 'remark--last-cursor-por)
    (make-variable-buffer-local 'remark--last-move-timer)
    (add-hook 'post-command-hook #'remark--post-command)))


(provide 'remark-mode)
;;; remark-mode.el ends here
