(setq user-mail-address "pkurlak@gmail.com")

;; Preferences
(setq inhibit-startup-message t)     ; Do without the annoying startup msg
(setq-default major-mode 'text-mode) ; Make text-mode the default for new buffers

;; Prevent the 'command attempted to use minibuffer while in minibuffer' error
;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Bootstrap straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
      (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

;; Packages
;;;; Evil-Leader provides an easy way to bind keys under a variable prefix
(straight-use-package 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
 "e"  'find-file
 "b"  'switch-to-buffer
 "nt" 'neotree-toggle)
 ;;"xb" 'kill-buffer
 ;;"xw" 'delete-window)

;;;; Evil replicates Vim's editing modes
(straight-use-package 'evil)
(evil-mode 1)
(eval-after-load "evil"
  '(progn
     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

;;;; Evil-Snipe emulates vim-sneak where the cursor can be quickly placed in the correct location
;;;; by searching for small snippets of text
(straight-use-package 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;;;; Evil-Multiedit allows you to select and edit multiple matches interactively
(straight-use-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

;;;; Ido enables better interactivity with buffers and files
(straight-use-package 'ido)
(ido-mode t)

;;;; Neotree duplicates Vim's NerdTree
(straight-use-package 'neotree)

;;;; Telephone-line is a new implementation of Powerline for emacs
(straight-use-package 'telephone-line)
(require 'telephone-line-config)
(telephone-line-evil-config)

;;;; Helm creates a dropdown window for narrowing search results
(straight-use-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;;;; Use monokai theme
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)
