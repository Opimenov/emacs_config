
;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/packages/")
;;macro for setting custom variables
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;;; There are few public repositories of Emacs packages. 
;;; I'm using melpa and you should too. I don't know why though.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; use-package is a nice package that simplifies package installation
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; there is a flavor of Emacs called spacemacs, 
;;; that uses space key instead of Ctrl. From what
;;; I could gather it is clunky, but the theme is nice.
(unless (package-installed-p 'spacemacs-theme)
   (package-refresh-contents)
   (package-install 'spacemacs-theme))

;;No need to describe this feature. Everybody knows it. Maybe not.
;;It allows you to have as many cursors on the screen as your 
;; CPU+GPU+Your-Head can handle. Allows you to use Dark Magic. 
(use-package multiple-cursors
  :ensure t
  :config
     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
     (define-key mc/keymap (kbd "<return>") nil))

;;Helm is modified help buffer. It attaches itself to a lot of actions
;;in Emacs environment and create more functional and better looking
;;help buffers. 
(use-package helm
  :ensure t
  :init
  (helm-mode 1))

;;install package first
(use-package helm-swoop
  :ensure t
  :config
     (global-set-key (kbd "M-x") 'helm-M-x)
     (global-set-key (kbd "M-i") 'helm-swoop);;helm search
     (global-set-key (kbd "M-u") 'helm-show-kill-ring);;BEST FEATURE EVER
     (global-set-key (kbd "C-c f") 'helm-locate));;find stuff quickly

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
   (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "<C-return>") 'shell)

(use-package org-bullets
   :ensure t
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq auto-save-default nil)
(setq make-backup-file nil)

(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode))

(when (fboundp 'winner-mode)
   (winner-mode 1))

(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode))

(use-package powerline
  :ensure t
  :init
  (powerline-default-theme))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq scroll-conservatively 100)
(setq ring-bell-function 'ignore)
(when window-system (global-hl-line-mode t))
(when window-system (global-prettify-symbols-mode t))
;;highlight cursor line on buffer opening
(use-package beacon
  :ensure t
  :init
  (beacon-mode -1))

;;clean up GUI GARBAGE
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

;;(use-package which-key
;;  :ensure t
;;  :init
;;  (which-key-mode))

(electric-pair-mode 1)

(desktop-save-mode 1)

;;if you are on Linux 
(use-package ediff
   :ensure t
   :init)

;;if you are on windouzzzzz os. Good luck. 
;; download cygwin-mount and setup-cygwin to "D:/Cygwin/bin"
;; Do you feel lucky today? try leaving package extensions.
;; best not to include the ending “.el” or “.elc” 
;;(load "cygwin-mount")
;;(load "setup-cygwin")

;;(setenv "PATH" (concat "D:/Cygwin/bin;" (getenv "PATH")))
;;(setq exec-path (cons "D:/Cygwin/bin" exec-path))
;;(require 'cygwin-mount)
;;(cygwin-mount-activate)

;;(csetq ediff-split-window-function 'split-window-horizontally)
;;(csetq ediff-diff-options "-w")
;;(csetq ediff-window-setup-function 'ediff-setup-windows-plain)

;;(winner-mode)
;;(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package s
  :ensure t
  :init)

(use-package dash
  :ensure t
  :init)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))
(require 'origami)

(set-face-attribute 'default nil :family "Consolas" :height 120)

(global-set-key (kbd "C-x C-r") (lambda () (interactive) (helm-swoop :$query "error:")))

(fset 'build-and-push-rs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([7 3 134217839 46 47 83 116 97 114 66 117 105 108 100 80 117 115 104 46 98 97 116] 0 "%d")) arg)))
(global-set-key (kbd "C-c s") 'build-and-push-rs)   
;;(fset 'star-build
;;   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 134217839 83 116 97 114 tab return] 0 "%d")) arg)))

(use-package magit
   :ensure t
   :init)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package helm-flyspell
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'helm-flyspell-correct))

(use-package sr-speedbar
  :ensure t
  :init)

(use-package pylint
  :ensure t
  :init)
(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)
