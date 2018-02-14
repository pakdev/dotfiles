(setq user-mail-address "pkurlak@gmail.com")

;; Preferences
(setq inhibit-startup-message t)     ; Do without the annoying startup msg
(setq-default major-mode 'text-mode) ; Make text-mode the default for new buffers

;;; Prevent the 'command attempted to use minibuffer while in minibuffer' error
;;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Package manager bootstrapping
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

;;; Evil replicates Vim's editing modes
(straight-use-package 'evil)
(evil-mode t)
;;(eval-after-load "evil"
;;  '(progn
;;     (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
;;     (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
;;     (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
;;     (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))
;;(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;;(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
;;(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;;(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;;; General provides an easy way to bind keys
(straight-use-package 'general)
;;(general-define-key
;; :keymaps 'normal
;; "j" 'evil-next-visual-line
;; "k" 'evil-previous-visual-line)
;;
;;(general-define-key
;; :prefix "SPC"
;; :keymaps 'normal
;; "nt" 'neotree-toggle)
;;
;;(general-define-key
;; :prefix "SPC"
;; :keymaps 'normal
;; "ff" 'find-file)
;;
;;(general-define-key
;; :prefix "SPC b"
;; :keymaps 'normal
;; "f" 'helm-mini
;; "x" 'kill-buffer)
;;
;;(general-define-key
;; :prefix "SPC w"
;; :keymaps 'normal
;; "x" 'delete-window)

;;; Evil-Snipe emulates vim-sneak where the cursor can be quickly placed in the correct location
;;; by searching for small snippets of text
(straight-use-package 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;;; Evil-Multiedit allows you to select and edit multiple matches interactively
(straight-use-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

;;; Ido enables better interactivity with buffers and files
(straight-use-package 'ido)
(ido-mode t)

;;; Neotree duplicates Vim's NerdTree
(straight-use-package 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;;; All-the-icons provides icons for neotree
;;;; First time initialization requires running M-x all-the-icons-install-fonts
(straight-use-package 'all-the-icons)

;;; Telephone-line is a new implementation of Powerline for emacs
(straight-use-package 'telephone-line)
(require 'telephone-line-config)
(telephone-line-evil-config)

;;; Helm creates a dropdown window for narrowing search results
(straight-use-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;;; Vimish-fold makes it easy to create folds on active regions
(straight-use-package 'vimish-fold)
;(global-set-key (kbd "<menu> v f") #'vimish-fold)
;(global-set-key (kbd "<menu> v v") #'vimish-fold-delete)

;;; Company-mode is a text completion framework for Emacs
(straight-use-package 'company)

;;; Org-mode is for keeping notes, maintaining TODO lists, planning projects, etc...
(straight-use-package '(org :local-repo nil))

;;; Elpy is for Python
(straight-use-package 'elpy)

;;; Use monokai theme
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)

;;; Customize Emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVuSansMonoForPowerline NF" :foundry "outline" :slant normal :weight normal :height 113 :width normal)))))
