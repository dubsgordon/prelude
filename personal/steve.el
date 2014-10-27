;;(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; mac keybinds
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;;disable this nonsense

(setq prelude-whitespace nil)
(setq prelude-flyspell nil)


(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; word-wrap is now off!
(setq-default truncate-lines 1)

(add-to-list 'load-path "~/.emacs.d/personal/nyan-mode")
(require 'nyan-mode)
(nyan-mode)

(require 'darcula-theme)

(require 'cl)
(loop for key in (list
                  (kbd "<wheel-down>")
                  (kbd "<double-wheel-down>")
                  (kbd "<triple-wheelf-down>")
                  (kbd "<wheel-up>")
                  (kbd "<double-wheel-up>")
                  (kbd "<triple-wheel-up>")
                  (kbd "S-<mouse-4>"))
      do (global-set-key key 'ignore))
(mouse-wheel-mode -1)


(global-linum-mode 0)

;; kill all dem javas!

(defun killall-java ()
  (interactive)
  (shell-command "killall java"))

(global-set-key (kbd "C-c C-v K") 'killall-java)

;; prefer backward kill word to region.
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;; inc number at point...
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(key-chord-define-global "bv" 'increment-number-at-point)

;; helm all the things...
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; prefer the visual bell
(setq visible-bell 1)

;; multiple cursors keybinds
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; yasnippits

(add-to-list 'load-path
             "~/.emacs.d/personal/yasnippet")
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(yas-global-mode 1)

(defun build-package-name (pn d)
  (if (null d)
      pn
    (let ((c (car d)))
      (if (equal c "scala")
          pn
        (build-package-name (concat c "." pn) (cdr d))))))

(defun scala-package-name-from-buffer ()
  (let ((l (reverse (split-string (buffer-file-name) "/"))))
    (build-package-name (cadr l) (cddr l))))


;; switch the vertical split to hors. and vice-a-versa
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


;; rotate the windows
(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))



;; This tries to find where the Spec/test of the current buffer lives, and either creates it, or visits it

(defun split-path-of-file (f)
"return dirname.filename" (let ((sp (reverse (split-string f "/"))))
(cons (mapconcat 'identity (reverse (cdr sp)) "/") (car sp))))

(defun scala-test-file-name (f)
  (let* ((sp (reverse (split-string f "\\.")))
         (h (car sp))
         (fn (cadr sp)))
    (mapconcat 'identity (reverse (cons h (cons (concat fn "Spec")(cddr sp)))) ".")))

(defun scala-find-src (sf d)
  (if (null d)
      sf
    (let ((c (car d)))
      (if (equal c "main")
          (append (reverse (cdr d)) (list "test") sf)
        (scala-find-src (cons c sf) (cdr d))))))

(defun scala-test-file-from-buffer ()
  (let* ((d (reverse (split-string (buffer-file-name) "/")))
        (test (scala-find-src nil d)))
    (scala-test-file-name (mapconcat 'identity test "/"))))

(defun scala-visit-spec ()
  (interactive)
  (let* ((tf (scala-test-file-from-buffer))
         (pf (split-path-of-file tf))
         (dn (car pf))
         (fn (concat dn "/" (cdr pf))))
    (if (file-exists-p dn)
        (find-file fn)
      (if (y-or-n-p (concat dn " doesn't exist, create it?"))
          (progn
            (mkdir dn t)
            (find-file fn))))))

(add-hook 'scala-mode-hook
            '(lambda ()
               (local-set-key (kbd "s-c t") `scala-visit-spec)))

(setq guru-warn-only t)
