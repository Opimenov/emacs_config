;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                              HERE BE DRAGONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This fixed garbage collection, makes emacs start up faster ;;;;;;;
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist startup/file-name-handler-alist))

(defun startup/reset-gc ()
  (setq gc-cons-threshold 16777216
	gc-cons-percentage 0.1))

(add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'startup/reset-gc)
;;Tell Emacs that you own it, and it should do what config.org says.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;;; Bootstrapping use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; This is the actual config file. It is omitted if
;;; it doesn't exist so emacs won't refuse to launch. 
(when (file-readable-p "~/.emacs.d/config.org")
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 3)
 '(ac-ispell-requires 3)
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(debug-on-error t)
 '(display-time-mode t)
 '(electric-pair-mode t)
 '(js-expr-indent-offset 0)
 '(line-number-mode nil)
 '(org-agenda-files (quote ("C:/Users/opimenov.EDC-SP/Desktop/TODOS.org")))
 '(package-selected-packages
   (quote
    (csharp-mode web-mode ac-helm helm-ac-php-apropos ac-php company-php php-mode yaml-mode company-c-headers flycheck-clang-analyzer flycheck yasnippet-snippets yasnippet dashboard xkcd diminish htmlize rainbow-mode switch-window sunrise-x-modeline sunrise-x-buttons sunrise-commander spaceline ox-twbs avy ac-ispell helm-projectile jedi company-jedi pretty-mode company-irony company s expand-region rainbow-delimiters rainbow-delimeters multiple-cursors indent-guide eshell-up powerline log4j-mode magit helm-config helm-swoop helm dired-sidebar org-bullets beacon spacemacs-theme dracula-theme which-key use-package)))
 '(python-shell-exec-path (quote ("C:/Python37")))
 '(python-shell-interpreter "C:/Python37/python")
 '(send-mail-function (quote mailclient-send-it))
 '(tool-bar-mode nil)
 '(treemacs-collapse-dirs 0)
 '(whitespace-style (quote (face tabs lines-tail newline empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "outline" :family "Consolas")))))

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
