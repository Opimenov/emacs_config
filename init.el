(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(unless (package-installed-p 'helm)
  (package-refresh-contents)
  (package-install 'helm))

(unless (package-installed-p 'helm-swoop)
  (package-refresh-contents)
  (package-install 'helm-swoop))

(unless (package-installed-p 'log4j-mode)
  (package-refresh-contents)
  (package-install 'log4j-mode))

(unless (package-installed-p 'ediff)
  (package-refresh-contents)
  (package-install 'ediff))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         DO NOT MODIFY BELOW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(electric-pair-mode t)
 '(package-selected-packages
   (quote
    (s expand-region rainbow-delimiters rainbow-delimeters multiple-cursors indent-guide eshell-up powerline log4j-mode magit helm-config helm-swoop helm dired-sidebar org-bullets beacon spacemacs-theme dracula-theme which-key use-package)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Consolas")))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
