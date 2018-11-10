
(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (define-key mc/keymap (kbd "<return>") nil))

(use-package helm
  :ensure t
  :init
  (helm-mode 1))

;;bind a key to help
(global-set-key (kbd "M-x") 'helm-M-x)

;;bind a key to help-swoop
(global-set-key (kbd "M-i") 'helm-swoop)

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
   (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;;HOW TO SETUP KEY BINDING TO RUN COMMANDS
;;customize this to use keyboard short cut to launch terminal or eshell
(global-set-key (kbd "<C-return>") 'shell)

(use-package org-bullets
   :ensure t
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq auto-save-default nil)
(setq make-backup-file nil)

(use-package expand-region
  :ensure t
  :init
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

;; add key binding if needed here
;;   (use-package ediff
;;      :ensure t
;;      :config
;;      ())))

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

(global-set-key (kbd "C-x g") 'magit-status)
