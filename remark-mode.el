(defun remark-next-slide ()
  "skip to next slide"
  (interactive)
  (search-forward-regexp "---" nil t)
  (if (looking-back "---")
      (progn (next-line)
             (move-beginning-of-line 1))
    (end-of-buffer)))

(defun remark-prev-slide ()
  "skip to prev slide"
  (interactive)
  (search-backward-regexp "---" nil t)
  (if (looking-at "---")
      (progn
        (previous-line)
        (move-beginning-of-line 1))
    (beginning-of-buffer)))

(defun remark-new-slide ()
  "creates new slide"
  (interactive)
  (remark-next-slide)
  (if (= (point) (point-max))
      (progn
        (insert "---\n")
        (next-line))
    (progn
      (previous-line 1)
      (insert "---\n\n")
      (previous-line))))

(defvar remark-mode-map (make-sparse-keymap) "remark-mode keymap")

(define-key remark-mode-map (kbd "C-c C-s n") 'remark-next-slide)
(define-key remark-mode-map (kbd "C-c C-s p") 'remark-prev-slide)
(define-key remark-mode-map (kbd "C-c C-s s") 'remark-new-slide)

(define-minor-mode remark-mode
  "remark mode"
  nil
  " remark"
  remark-mode-map)

(provide 'remark-mode)