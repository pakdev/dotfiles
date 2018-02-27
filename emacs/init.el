(setq user-mail-address "pkurlak@gmail.com")

;; [Windows configuration](https://www.johndcook.com/blog/emacs_windows/)
;; Avoid "Emacs droppings"
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*",temporary-file-directory t)))

;; Better defaults from https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#hydra
(set-frame-parameter nil 'fullscreen 'fullboth) ; Enable fullscreen
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
 confirm-kill-emacs nil               ; Confirm before exiting Emacs
 cursor-in-non-selected-windows nil   ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t          ; Delete files to trash
 inhibit-startup-message t            ; Disable start-up screen
 display-time-format "%H:%M"          ; Format the time string
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
(global-linum-mode t)                 ; Enable line numbers
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
 :keymaps '(normal visual emacs motion)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 "" 'hydra-menu/body)

(defun mode-specific-actions ()
  (interactive)
  (cond
   ((string= major-mode "python-mode")
    (hydra-python-mode/body))
   ((or (string= major-mode "lisp-interaction-mode")
        (string= major-mode "emacs-lisp-mode"))
    (hydra-lisp-mode/body))
   (t
    (progn
      (print major-mode)
      (hydra-menu/body)))))

(defhydra hydra-menu (:color blue)
  "
"
  ("w" hydra-windows/body "+windows" :column "Navigation")
  ("b" hydra-buffers/body "+buffers")
  ("f" hydra-files/body "+files")
  ("z" hydra-zoom/body "+zoom" :column "Actions")
  ("m" mode-specific-actions "+mode")
  ("q" nil "quit" :column nil))

(defhydra hydra-windows (:color red)
  "
"
  ("h" windmove-left  "←" :column "Navigation")
  ("j" windmove-down  "↓")
  ("k" windmove-up    "↑")
  ("l" windmove-right "→")
  ("a" ace-select-window "ace select")
  ("s" split-window-below "split" :color blue :column "Actions")
  ("v" split-window-right "split vertically" :color blue)
  ("d" delete-window "delete")
  ("f" follow-mode "toggle follow mode")
  ("u" winner-undo "undo window conf" :column "Undo/Redo")
  ("r" winner-redo "redo window conf")
  ("b" balance-windows "balance heights" :column "Sizing")
  ("m" maximize-window "maximize current")
  ("M" minimize-window "minimize current")
  ("q" nil "quit" :color blue :column nil))

(defhydra hydra-buffers (:color red)
  "
"
  ("j" evil-next-buffer "↓" :column "Navigation")
  ("k" evil-prev-buffer "↑")
  ("d" bjm/kill-this-buffer "delete" :column "Actions")
  ("q" nil "quit" :color blue :column nil))

(defhydra hydra-files (:color blue)
  "
"
  ("f" find-file "find" :column "Navigation")
  ("e" hydra-init-file/body "act on init" :column "Actions")
  ("d" delete-file "delete")
  ("q" nil "quit" :column nil))

(defhydra hydra-init-file (:color blue)
  "
"
  ("d" edit-init-file "edit" :column "Actions")
  ("R" reload-init-file "reload")
  ("q" nil "quit" :column nil))

;(defhydra hydra-spaces (:color blue)
;  "
;  _d_ delete  _j_ next  _k_ prev  _l_ last  _r_ rename
;  "
;  ("d" eyebrowse-close-window-config)
;  ("j" eyebrowse-next-window-config)
;  ("k" eyebrowse-prev-window-config)
;  ("l" eyebrowse-last-window-config)
;  ("r" eyebrowse-rename-window-config)
;  ("q" nil))

(defhydra hydra-zoom (:color red)
  "Text Zoom"
  ("+" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("q" nil "quit" :color blue))

(defhydra hydra-python-mode (:color blue)
  "Python"
  ("v" (venv-workon) "set venv" :column "Init")
  ("p" hydra-python-mode-project/body "project")
  ("e" hydra-python-mode-eval/body "eval" :column "Actions")
  ("c" hydra-python-mode-check/body "check")
  ("t" elpy-test "test [case]")
  ("h" elpy-doc "show doc")
  ("Q" (kill-buffer "*compilation*") "quit and kill compilation buffer" :column nil)
  ("q" nil "quit" :column nil))

(defhydra hydra-python-mode-project (:color blue)
  "Python Project"
  ("r" elpy-set-project-root "set root")
  ("f" elpy-find-file "file" :column "Search")
  ("g" elpy-rgrep-symbol "symbol")
  ("q" nil "quit" :column nil))

(defhydra hydra-python-mode-check (:color blue)
  "Python Linting"
  ("x" elpy-check "execute")
  ("n" next-error "next error" :color red :column "Navigation")
  ("p" previous-error "prev error" :color red)
  ("q" nil "quit" :column nil))

(defhydra hydra-python-mode-eval (:color blue)
  "Python Evaluation"
  ("f" elpy-shell-send-defun "function")
  ("F" elpy-shell-send-defun-and-go "function, REPL")
  ("r" elpy-shell-send-region-or-buffer "region")
  ("R" elpy-shell-send-region-or-buffer-and-go "region, REPL")
  ("b" elpy-shell-send-buffer "buffer")
  ("B" elpy-shell-send-buffer-and-go "buffer, REPL")
  ("q" nil "quit" :column nil))

(defhydra hydra-lisp-mode (:color blue)
  "Lisp"
  ("r" eval-region "region" :column "Evaluation")
  ("b" eval-buffer "buffer")
  ("s" eval-last-sexp "last sexp")
  ("q" nil "quit" :column nil))

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
(elpy-enable)

;; [historian](https://github.com/PythonNut/historian.el)
(straight-use-package 'historian)

;; [eyebrowse](https://github.com/wasamasa/eyebrowse)
(straight-use-package 'eyebrowse)

;; [ace-window](https://github.com/abo-abo/ace-window)
(straight-use-package 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Appearance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use monokai theme
(straight-use-package 'monokai-theme)
(load-theme 'monokai t)

;; Change font and size
(defvar windows-font "DejaVu Sans Mono")
(defvar windows-font-size 13)
(defvar linux-font "DejaVu Sans Mono")
(defvar linux-font-size 13)
(cond
 ((string= system-type "windows-nt") ; Microsoft Windows
  (when (member windows-font (font-family-list))
    (set-face-attribute 'default nil :font (format "%s-%s" windows-font windows-font-size))))
 ((string= system-type "gnu/linux")  ; Linux
  (when (member linux-font (font-family-list))
    (set-face-attribute 'default nil :font (format "%s-%s" linux-font linux-font-size)))))
