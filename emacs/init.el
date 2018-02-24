(setq user-mail-address "pkurlak@gmail.com")

;; [Windows configuration](https://www.johndcook.com/blog/emacs_windows/)
;; Avoid "Emacs droppings"
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*",temporary-file-directory t)))

;; Better defaults from https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra
;(set-frame-parameter nil 'fullscreen nil) ; Enable fullscreen
(when window-system
  (blink-cursor-mode 0)  ; Disable cursor blinking
  (scroll-bar-mode 0)    ; Disable the scroll bar
  (tool-bar-mode 0)      ; Disable the tool bar
  (menu-bar-mode 0)      ; Disable the menu bar
  (tooltip-mode 0))      ; Disable tooltips

(cua-mode t)                          ; Allow normal copy/paste behavior
(setq cua-keep-region-after-copy t)   ; Standard Windows behavior
(setq-default major-mode 'text-mode)  ; Make text-mode the default for new buffers
(setq-default
 ad-redefintion-action 'accept        ; Silence warnings for redefinition
 confirm-kill-emacs 'yes-or-no-p      ; Confirm before exiting Emacs
 cursor-in-non-selected-windows t     ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t          ; Delete files to trash
 inhibit-startup-message t            ; Disable start-up screen
 display-time-format "%H:%M"         ; Format the time string
 indent-tabs-mode nil                 ; Stop using tabs to indent
 initial-scratch-message ""           ; Empty the initial *scratch* buffer
 ns-use-srgb-colorspace nil           ; Don't use sRGB colors
 left-margin-width 1                  ; Add left and right margins
 right-margin-width 1
 mode-require-final-newline 'visit    ; Add a newline at EOF on visit
 mouse-yank-at-point t                ; Yank at point rather than pointer
 select-enable-clipboard t            ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil        ; End a sentence after a dot and a space
 show-trailing-whitespace t           ; Show trailing whitespace
 tab-width 4                          ; Default to 4 spaces for tabs
 x-stretch-cursor t)                  ; Stretch cursor to the glyph width
(delete-selection-mode)               ; Replace region when inserting text
(display-time-mode)                   ; Show time in the mode-line
(fringe-mode 0)                       ; Disable fringes
(fset 'yes-or-no-p 'y-or-n-p)         ; Replace yes/no prompts with y/n
(global-hl-line-mode)                 ; Highlight current line
(global-subword-mode)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'banish)        ; Avoid collision of mouse with point
(cd "~/")                             ; Start at the user directory

(add-hook 'focus-out-hook #'garbage-collect)

;; http://pragmaticemacs.com/emacs/dont-kill-buffer-kill-this-buffer-instead/
(defun bjm/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun edit-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun reload-init-file ()
  "Reload the init file."
  (interactive)
  (load-file user-init-file))

;; Prevent the 'command attempted to use minibuffer while in minibuffer' error
;; Taken from: https://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [evil](https://www.emacswiki.org/emacs/Evil)
(setq evil-want-integration nil)
(straight-use-package 'evil)
(evil-mode t)
;; Make all modes start in normal mode
(setq evil-emacs-state-modes nil)
(setq evil-insert-state-modes nil)
(setq evil-motion-state-modes nil)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))  ; Make "ESC" quit the minibuffer

;; [evil-collection](https://github.com/jojojames/evil-collection)
(straight-use-package 'evil-collection)
(evil-collection-init)

;; [general](https://github.com/noctuid/general.el)
(straight-use-package 'general)
(general-define-key
 :keymaps 'insert
 "C-c" 'cua-copy-region
 "C-v" 'cua-paste
 "C-x" 'cua-cut-region
 "C-z" 'undo-tree-undo
 "C-y" 'undo-tree-redo
 )

;; [hydra](https://github.com/abo-abo/hydra)
(straight-use-package 'hydra)
(general-define-key
 :keymaps 'normal
 :prefix "SPC"
 "" 'hydra-menu/body)

(defhydra hydra-menu (:color blue)
  "
  _w_ +windows  _b_ +buffers  _f_ +files  _s_ +spaces  _a_ +actions
  "
  ("w" hydra-windows/body)
  ("b" hydra-buffers/body)
  ("f" hydra-files/body)
  ("s" hydra-spaces/body)
  ("a" hydra-actions/body)
  ("q" nil))

(defhydra hydra-windows (:color blue)
  "
  _d_ delete  _h_ move left  _j_ move down  _k_ move up  _l_ move right
  "
  ("d" delete-window)
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("q" nil))

(defhydra hydra-spaces (:color blue)
  "
  _d_ delete  _j_ next  _k_ prev  _l_ last  _r_ rename
  "
  ("d" eyebrowse-close-window-config)
  ("j" eyebrowse-next-window-config)
  ("k" eyebrowse-prev-window-config)
  ("l" eyebrowse-last-window-config)
  ("r" eyebrowse-rename-window-config)
  ("q" nil))

(defhydra hydra-buffers (:color blue)
  "
  _d_ delete  _j_ next  _k_ prev
  "
  ("d" bjm/kill-this-buffer)
  ("j" evil-next-buffer)
  ("k" evil-prev-buffer)
  ("q" nil))

(defhydra hydra-files (:color blue)
  "
  _d_ delete  _f_ find  _e_ +edit
  "
  ("d" delete-file)
  ("f" find-file)
  ("e" hydra-edit-files/body)
  ("q" nil))

(defhydra hydra-edit-files (:color blue)
  "
  _d_ init  _R_ reload
  "
  ("d" edit-init-file)
  ("R" reload-init-file)
  ("q" nil))

(defhydra hydra-actions (:color blue)
  "
  _+_ zoom-in  _-_ zoom-out
  "
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("q" nil))

;; [evil-multiedit](https://github.com/hlissner/evil-multiedit)
(straight-use-package 'evil-multiedit)
(require 'evil-multiedit)
(evil-multiedit-default-keybinds)

;; [ido](https://www.emacswiki.org/emacs/InteractivelyDoThings)
(straight-use-package 'ido)
(ido-mode t)
(ido-everywhere t)

;; [flx-ido](https://github.com/lewang/flx)
(straight-use-package 'flx-ido)
(flx-ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; [projectile](http://batsov.com/projectile/)
(straight-use-package 'projectile)
(setq projectile-indexing-method 'alien)

(straight-use-package 'all-the-icons)

;; [neotree](https://github.com/jaypei/emacs-neotree)
;; - First time initialization requires running M-x all-the-icons-install-fonts
(straight-use-package 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-smart-open t) ; jump to node of current file
(setq projectile-switch-project-action 'neotree-projectile-action) ; change root if projectile project is changed
(setq inhibit-compacting-font-caches t) ; make all-the-icons faster on Windows (https://github.com/domtronn/all-the-icons.el/issues/28)

;; [telephone-line](https://github.com/dbordak/telephone-line)
(straight-use-package 'telephone-line)
(require 'telephone-line-config)
(telephone-line-evil-config)

;; [helm](https://github.com/emacs-helm/helm)
(straight-use-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq-default helm-display-header-line nil)

;; [vimish-fold](https://github.com/mrkkrp/vimish-fold)
(straight-use-package 'vimish-fold)
;(global-set-key (kbd "<menu> v f") #'vimish-fold)
;(global-set-key (kbd "<menu> v v") #'vimish-fold-delete)

;; [company-mode](https://company-mode.github.io/)
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; [org-mode](https://orgmode.org/)
(straight-use-package '(org :local-repo nil))

;; [python-mode](https://github.com/jorgenschaefer/elpy)
(straight-use-package 'elpy)

;; [historian](https://github.com/PythonNut/historian.el)
(straight-use-package 'historian)

;; [eyebrowse](https://github.com/wasamasa/eyebrowse)
(straight-use-package 'eyebrowse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use monokai theme
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)

;; Customize Emacs
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
