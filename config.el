;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/packages/")
;;macro for setting custom variables
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(server-start)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("ELPA"  . "http://tromey.com/elpa/")
   		      ("gnu"   . "http://elpa.gnu.org/packages/")
   		      ("melpa" . "https://melpa.org/packages/")
   		      ("org"   . "https://orgmode.org/elpa/")
   		      ))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

(defun keyboard-quit-only-if-no-macro ()
  (interactive)
  (if defining-kbd-macro
      (progn
        (if (region-active-p)
            (deactivate-mark)
          (message "Macro running. Can't quit.")))

    (keyboard-quit)))

(define-key global-map (kbd "C-g") 'keyboard-quit-only-if-no-macro)

(use-package helm
    :ensure t
    :init
    (helm-mode 1))

  ;;install package first
  (use-package helm-swoop
    :ensure t
    :config
       (global-set-key (kbd "M-x") 'helm-M-x)
       (global-set-key (kbd "M-i") 'helm-swoop);;helm search FIXME
       (global-set-key (kbd "M-u") 'helm-show-kill-ring);;BEST FEATURE EVER
       (global-set-key (kbd "C-x C-f") 'helm-find-files);;find stuff quickly
;;       (global-set-key (kbd "C-c f f") 'helm-projectile)
;;       (global-set-key (kbd "C-c g") 'helm-projectile-grep)
       (global-set-key (kbd "C-c i") 'helm-imenu)) ;;go to function name quickly

  (global-set-key (kbd "C-c p") 'projectile-find-file)

(setq shell-file-name "/usr/bin/zsh")
(setq explicit-bash.exe-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(defadvice ansi-term (before force-zshell)
  (interactive (list my-term-shell)))
 (ad-activate 'ansi-term)
(prefer-coding-system 'utf-8)

;;(global-set-key (kbd "<C-x  e>") 'eshell)
;;startup shell on boot
;;(eshell)

(setq org-ellipsis " ")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-export-with-smart-quotes t)
(setq org-src-window-setup 'current-window)
(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-agenda-files (list "/home/pimenov/Desktop/notes.org"))
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
      (quote ((sequence "TODO(t)" "WAITING(w)" "WORKING_ON(p)" "UNDER_REVIEW(r)" "FIX_VERIFICATION(f)" "DONE(d)"))))
(setq org-log-done t)

(use-package ox-twbs
  :ensure t)

;; (setenv "PATH" (concat (getenv "PATH") ":D:/Installed_progs/MiKTex/"))
;;     (setq exec-path (append exec-path '("D:/Installed_progs/MiKTex/")))

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

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))

;; use bind-key package to override major mode key maps
(bind-key*  "C-," 'windmove-left)
(bind-key*  "C-." 'windmove-right)
(bind-key*  "M-P" 'windmove-up)
(bind-key*  "M-N" 'windmove-down)

(setq tramp-copy-size-limit nil)
(setq tramp-inline-compress-start-size nil)

;;(setq auto-save-default nil) might not be a good idea after all.
(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))
;;(setq make-backup-file nil)

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

;;do not touch
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           (use-package spaceline
             :ensure t
             :config
             (require 'spaceline-config)
               (setq spaceline-buffer-encoding-abbrev-p nil)
               (setq spaceline-line-column-p nil)
               (setq spaceline-line-p nil)
               (setq powerline-default-separator nil)
               ;;(spaceline-spacemacs-theme)
               (powerline-center-theme))

          ;; (use-package powerline
          ;;  :ensure t
          ;;  :config
          ;;  (setq powerline-default-separator nil)
          ;;  (powerline-center-theme))
    ;;  (use-package smart-mode-line
    ;;       :ensure t)
    ;;  (setq powerline-default-separator nil)
    ;;  (sml/setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(setq inhibit-startup-screen t
    initial-buffer-choice  nil)

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
  (use-package ediff
     :ensure t
     :init)
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
;;  (load "cygwin-mount")
;;  (load "setup-cygwin")

;;  (setenv "PATH" (concat ":D:/Cygwin/bin;" (getenv "PATH")))
;;  (setq exec-path (cons "D:/Cygwin/bin" exec-path))
;;  (setenv "PATH" (concat ":D:/Installed_progs/Git/bin;" (getenv "PATH")))
;;  (setq exec-path (cons "D:/Installed_progs/Git/bin" exec-path))
;;  (require 'cygwin-mount)
;;  (cygwin-mount-activate)

  (csetq ediff-split-window-function 'split-window-horizontally)
  (csetq ediff-diff-options "-w")
  (csetq ediff-window-setup-function 'ediff-setup-windows-plain)

  (winner-mode 1)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(use-package s
  :ensure t
  :init)

(use-package dash
  :ensure t
  :init)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/packages/"))
(require 'origami)

(use-package mines
  :ensure t)

;; Set default font
;;(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)

(use-package magit
   :ensure t
   :init
   :config
(global-set-key (kbd "C-x g") 'magit-status)
(magit-auto-revert-mode -1))
;;there are multiple git versions installed so lets point to this one 
;;(setq magit-git-executable "d:/Installed_progs/Git/bin/git.exe")

(require 'smithy-mode)
(add-to-list 'auto-mode-alist
             '("\\.smithy\\'" . (lambda ()
                                  (smithy-mode))))

;;(setq ispell-alternate-dictionary (file-truename "~/.emacs.d/misc/english-words.txt"))
;;(setq ispell-program-name "aspell")
;;(use-package ac-ispell
;;   :ensure t
;;   :init)
;; Completion words longer than 4 characters
    ;; (custom-set-variables
    ;;   '(ac-ispell-requires 3)
    ;;   '(ac-ispell-fuzzy-limit 3))

    ;; (eval-after-load "auto-complete"
    ;;   '(progn
    ;;       (ac-ispell-setup)))

    ;; (add-hook 'git-commit-mode-hook 'ac-ispell-ac-setup)
    ;; (add-hook 'mail-mode-hook 'ac-ispell-ac-setup)
    ;; (add-hook 'org-mode-hook 'ac-ispell-ac-setup)
     (use-package helm-flyspell
       :ensure t
       :config
       (global-set-key (kbd "C-:") 'helm-flyspell-correct))

;;(setq display-time-24hr-format t)
;;(setq display-time-format "%H:%M - %d %B %Y")

(display-time-mode nil)

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
    (diminish 'follow-mode)
    (diminish 'yas-minor-mode)
    (diminish 'abbrev-mode)
    (diminish 'whitespace-mode)
    (diminish 'text-scale-mode)
    (diminish 'indent-guide-mode)
    (diminish 'follow-mode)
    (diminish 'org-indent-mode)
    (diminish 'auto-complete-mode)
    (diminish 'eldoc-mode)
    (diminish 'org-indent-mode)
    (diminish 'text-scale-mode)
    (diminish 'company-mode)
    (diminish 'org-indent-mode)
    (diminish 'scroll-all-mode)
    (diminish 'subword-mode)
    (diminish 'projectile-mode)
)

(use-package xkcd
  :ensure t)

;;   (use-package dashboard
;;     :ensure t
;;     :config
;;       (dashboard-setup-startup-hook)
;; ;;      (setq dashboard-startup-banner "C:/Users/opimenov.EDC-SP/Desktop/presentations/dusty_pc.png")
;;       (setq dashboard-items '((recents  . 5)
;;                               (projects . 5)
;;                               (agenda . 5)))
;;       (setq dashboard-banner-logo-title ""))

;; ;; to update cached xkcd
;; (with-temp-buffer
;;   (xkcd)
;;   (xkcd-kill-buffer))

;; ;; setting dashboard image (png)
;; (let ((last-xkcd-png (concat xkcd-cache-dir (number-to-string xkcd-latest) ".png")))
;;   (if (file-exists-p last-xkcd-png)
;;       (setq dashboard-banner-official-png last-xkcd-png)))

;; ;; to get a rand comic and to set
;;dashboard image (png)
;;(let ((rand-id-xkcd nil))
;;  (with-temp-buffer
;;    (setq rand-id-xkcd (string-to-number (xkcd-rand)))
;;    (xkcd-kill-buffer))
;;  (let ((last-xkcd-png (concat xkcd-cache-dir (number-to-string rand-id-xkcd) ".png")))
;;    (if (file-exists-p last-xkcd-png)
;;    (setq dashboard-banner-official-png last-xkcd-png))))

;; (add-hook 'dired-mode-hook
   ;;       (lambda ()
   ;;         (dired-sort-toggle-or-edit)
   ;;         (dired-hide-details-mode)    
   ;;       )
   ;; )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; stopped working at some point 
  ;;(load "dired+")                             
  ;;(global-dired-hide-details-mode t)          
  ;;(setq diredp-hide-details-initially-flag t) 
  ;;(setq diredp-hide-details-propagate-flag t) 
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq tramp-verbose 10)

(use-package format-all
  :ensure t)

(fset 'indent_after_70_chars
   (kmacro-lambda-form [?\C-u ?7 ?0 ?\C-f ?\M-f return ?\C-d] 0 "%d"))

(defun crux-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (let ((value (eval (elisp--preceding-sexp))))
    (backward-kill-sexp)
    (insert (format "%S" value))))
(global-set-key (kbd "C-x C-e") 'crux-eval-and-replace)

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))

;;(add-hook 'prog-mode-hook 'whitespace-mode)

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

(use-package yaml-mode
  :ensure t
  :config
   (add-hook 'yaml-mode-hook
    '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package typescript-mode
  :ensure t)

(setenv "PATH" (concat (getenv "PATH") ":/home/pimenov/.toolbox/bin"))
    (setq exec-path (append exec-path '("/home/pimenov/.toolbox/bin")))
    (use-package amz-common
      :ensure t)
    (use-package amz-crisp
      :ensure t)
  (define-key amz-crisp-mode-map (kbd "C-c x") 'amz-crisp)
  (setq amz-crisp-use-comint t)
