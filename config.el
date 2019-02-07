;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/packages/")
(add-to-list 'load-path "~/.emacs.d/packages/sidebar/")
;;macro for setting custom variables
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

;; (require 'package)
;; (setq package-enable-at-startup nil)
;; (setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
;; 			 ("gnu"   . "http://elpa.gnu.org/packages/")
;; 			 ("melpa" . "https://melpa.org/packages/")
;; 			 ("org"   . "https://orgmode.org/elpa/")
;; 			 ("SC"    . "http://joseito.republika.pl/sunrise-commander/")))
;; (package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
   (package-refresh-contents)
   (package-install 'spacemacs-theme))

(use-package multiple-cursors
  :ensure t
  :config
     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
     (define-key mc/keymap (kbd "<return>") nil))

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
     (global-set-key (kbd "C-x C-f") 'helm-find-files));;find stuff quickly

(defvar my-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
   (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(global-set-key (kbd "<C-return>") 'shell)
(eshell)

(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files (list "C:/Users/opimenov/Desktop/TODOS.org"))
(global-set-key (kbd "C-c s l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(setq org-log-done t)

(add-to-list 'org-structure-template-alist
	       '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(use-package htmlize
  :ensure t)

(use-package org-bullets
   :ensure t
   :config
   (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq org-log-done 'time)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "WAITING(w)" "DONE(d)"))))
(setq org-log-done t)

(use-package ox-twbs
  :ensure t)

(setenv "PATH" (concat (getenv "PATH") ":C:/Users/opimenov/AppData/Local/Programs/MiKTeX 2.9/"))
    (setq exec-path (append exec-path '("C:/Users/opimenov/AppData/Local/Programs/MiKTeX 2.9/")))

; allow for export=>beamer by placing

;; #+LaTeX_CLASS: beamer in org files
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  ;; beamer class, for presentations
  '("beamer"
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n"

     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}"
       "\\begin{frame}[fragile]\\frametitle{%s}"
       "\\end{frame}")))

  ;; letter class, for formal letters

  (add-to-list 'org-export-latex-classes

  '("letter"
     "\\documentclass[11pt]{letter}\n
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{color}"
     
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(use-package pretty-mode
    :ensure t
    :config
    (global-pretty-mode 1))

(setq auto-save-default nil)
(setq make-backup-file nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package async
 :ensure t
 :init (dired-async-mode 1))

(setq-default indent-tabs-mode nil)

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(defun alex_commands_to_kill_this_word ()
  "Kills the entire word your cursor is in."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w k") 'alex_commands_to_kill_this_word)

(defun alex_commads_to_copy_whole_word ()
  (interactive)
  (save-excursion 
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "C-c w c") 'alex_commads_to_copy_whole_word)

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c l c") 'daedreth/copy-whole-line)

(global-set-key (kbd "C-c l k") 'kill-whole-line)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  "Reloads ~/.emacs.d/config.org at runtime"
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(use-package rainbow-delimiters
  :ensure t
  :init
  (rainbow-delimiters-mode 1))

(use-package rainbow-mode
  :ensure t
  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

(when (fboundp 'winner-mode)
   (winner-mode 1))

(use-package indent-guide
  :ensure t
  :init
  (indent-guide-global-mode))

;; (use-package spaceline
    ;;   :ensure t
    ;;   :config
    ;;   (require 'spaceline-config)
    ;;     (setq spaceline-buffer-encoding-abbrev-p nil)
    ;;     (setq spaceline-line-column-p nil)
    ;;     (setq spaceline-line-p nil)
    ;;     (setq powerline-default-separator (quote arrow))
    ;;     (spaceline-spacemacs-theme))

    ;; (use-package powerline
    ;;  :ensure t
    ;;  :config
    ;;  (setq powerline-default-separator nil)
    ;;  (powerline-center-theme))
(use-package smart-mode-line
     :ensure t)
(setq powerline-default-separator nil)
(sml/setup)

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
(global-linum-mode -1)
(linum-mode -1)
(setq inhibit-startup-message t)

;;(use-package which-key
;;  :ensure t
;;  :init
;;  (which-key-mode))

(electric-pair-mode 1)

(desktop-save-mode 1)

(use-package avy
   :ensure t
   :config
    (global-set-key (kbd "M-s") 'avy-goto-char))

;;if you are on Linux 
;;(use-package ediff
;;   :ensure t
;;   :init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;if you are on WINDOUZZZZZ OS. Good luck.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; download cygwin-mount and setup-cygwin to "D:/Cygwin/bin"
;; if you don't have a D drive or want to have it some place
;; else you'll need to replace the path. Search for the path
;; that I have and replace it.
;; Do you feel lucky today? try leaving package extensions.
;; best not to include the ending “.el” or “.elc” 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMENT OUT THE REST OF THE SET UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "cygwin-mount")
(load "setup-cygwin")

(setenv "PATH" (concat "D:/Cygwin/bin;" (getenv "PATH")))
(setq exec-path (cons "D:/Cygwin/bin" exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (winner-mode 1)
;; (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; (use-package s
;;   :ensure t
;;   :init)

;; (use-package dash
;;   :ensure t
;;   :init)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))
;; (require 'origami)

(set-face-attribute 'default nil :family "Consolas" :height 120)

(global-set-key (kbd "C-x C-r") (lambda () (interactive) (helm-swoop :$query "error:")))

;; (fset 'build-and-push-rs
;;    (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([7 3 134217839 46 47 83 116 97 114 66 117 105 108 100 80 117 115 104 46 98 97 116] 0 "%d")) arg)))
;; (global-set-key (kbd "C-c s") 'build-and-push-rs)   
;;(fset 'star-build
;;   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 134217839 83 116 97 114 tab return] 0 "%d")) arg)))

(use-package magit
   :ensure t
   :init)
(global-set-key (kbd "C-x g") 'magit-status)

(setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt"))
(setq ispell-program-name "aspell")
(use-package ac-ispell
   :ensure t
   :init)
  ;; Completion words longer than 4 characters
    (custom-set-variables
      '(ac-ispell-requires 3)
      '(ac-ispell-fuzzy-limit 3))

    (eval-after-load "auto-complete"
      '(progn
          (ac-ispell-setup)))

    (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
    (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
    (add-hook 'org-mode-hook 'ac-ispell-ac-setup)
     (use-package helm-flyspell
       :ensure t
       :config
       (global-set-key (kbd "C-:") 'helm-flyspell-correct))

(use-package sr-speedbar
  :ensure t
  :init)

(setq display-time-24hr-format t)
(setq display-time-format "%H:%M - %d %B %Y")

(display-time-mode 1)

(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-current-buffer ()
  "Kills the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-c k k") 'kill-current-buffer)

(defun close-all-buffers ()
  "Kill all buffers without regard for their origin."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-S-k") 'close-all-buffers)

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'rainbow-mode)
  (diminish 'helm-mode)
  (diminish 'projectile-mode)
  (diminish 'indent-guide-mode))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-git-command-pipe           ""
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-max-git-entries            5000
          treemacs-no-png-images              nil
          treemacs-no-delete-other-windows    t
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-cursor                nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(treemacs)

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clang-analyzer)
     (flycheck-clang-analyzer-setup)))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package company-c-headers
  :ensure t)

   ;; Windows performance tweaks
   ;;
   (when (boundp 'w32-pipe-read-delay)
     (setq w32-pipe-read-delay 0))
   ;; Set the buffer size to 64K on Windows (from the original 4K)
   (when (boundp 'w32-pipe-buffer-size)
     (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(use-package company-irony
  :ensure t
  :config
  (setq company-backends '((company-c-headers
                            company-dabbrev-code
                            company-irony))))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))
