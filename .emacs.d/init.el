;;; init.el --- paraparity's Emacs configuration file
;;
;;; Commentary:
;; Customized init.el for Emacs
;;
;;; Code:
;;(setq debug-on-error t)

;; Eternally grateful to have gleaned lessons from:
;;   http://pages.sachachua.com/.emacs.d/Sacha.html
;;   https://github.com/purcell/emacs.d
;;   https://github.com/howardabrams/dot-files
;;   https://github.com/skeeto/.emacs.d

;; test

(setq enable-local-eval nil
	  enable-local-variables nil)

;; Configure load path
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/elisp")

;; Encoding
(set-language-environment "japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Configure Package Manager
(when (>= emacs-major-version 24)
  (require 'package)
  (defvar package-enabled-at-startup nil)
  (unless (assoc-default "melpa" package-archives)
	(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")    t))
  (unless (assoc-default "gnu" package-archives)
	(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t))
  (unless (assoc-default "org" package-archives)
	(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/")      t))
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq abbrev-file-name
		"~/.emacs.d/abbrev_defs")
  (setq save-abbrevs t)
  (if (file-exists-p abbrev-file-name)
	  (quietly-read-abbrev-file)))

;; LSP mode configuration
(use-package lsp-mode
  :commands lsp
  :hook (c++-mode python-mode go-mode-hook)
  :init
  (setq lsp-prefer-flymake nil))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)

(use-package cperl-mode)

(use-package erc
  :ensure nil
  :defer t
  :config
  (setq erc-hide-list '("PART" "QUIT" "JOIN" "NICK")
		erc-autojoin-channels-alist	'(("freenode.net"
									   "##programming"
									   "#emacs"
									   "#emacs-beginners"
									   "#org-mode"
									   "#ledger"
									   "#plaintextaccounting"))
		erc-server "irc.freenode.net"
		erc-nick "paraparity"
		erc-kill-buffer-on-part t
		erc-scrolltobottom-mode t))

(use-package flycheck
  :ensure t)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

 (use-package ledger-mode
   :mode ("\\.ledger$")
   :bind (:map ledger-mode-map
			   ("C-x C-s" . my/ledger-save))
   :preface
   (defun my/ledger-save ()
	 "Automatically clean the ledger buffer at each save."
	 (interactive)
	 (save-excursion
	   (when (buffer-modified-p)
		 (with-demoted-errors (ledger-mode-clean-buffer))
		 (save-buffer))))
   :init
   (setq ledger-post-amount-alignment-column 120))

(use-package flycheck-ledger
  :after ledger-mode)

(use-package multiple-cursors
  :ensure t
  :bind (;; Note that recomended 'C->' and 'C-<' are not characters in the shell.
		 ;; Thus I use their lowercase alternatives
		 ("C-c ."   . mc/mark-next-like-this)
		 ("C-c ,"   . mc/mark-previous-like-this)
		 ("C-c /"   . mc/mark-all-like-this)
		 ("C-c m m" . mc/mark-all-like-this-dwim)
		 ("C-c m a" . mc/edit-beginnings-of-lines)
		 ("C-c m e" . mc/edit-ends-of-lines)
		 ("C-c m s" . mc/mark-sgml-tag-pair)
		 ("C-c m l" . mc/edit-lines)
		 ))

(use-package ob-C)
(use-package ob-ditaa)
(use-package ob-dot)
(use-package ob-js)
(use-package ob-perl)
(use-package ob-plantuml)
(use-package ob-sql)
(use-package ox-html)
(use-package ox-latex)
(use-package ox-ascii)
(use-package htmlize
  :ensure t)
(use-package sql)
(use-package tramp)
(use-package uniquify)

(use-package company
  :ensure t
  :diminish
  :bind (:map company-active-map
			  ("M-n" . nil)
			  ("M-p" . nil)
			  ("C-n" . company-select-next)
			  ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay            nil)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers          t)
  (setq company-tooltip-limit         20)
  (with-eval-after-load 'company
	(global-set-key (kbd "C-c SPC") 'company-complete)))

(use-package company-lsp
  :after (company lsp-mode)
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(use-package helm
  :diminish helm-mode
  :init (progn
		  (require 'helm-config)
		  (helm-mode))
  :bind (("C-c h"   . helm-command-prefix)
		 ("M-x"     . helm-M-x)
		 ("M-y"     . helm-show-kill-ring)
		 ("C-x b"   . helm-mini)
		 ("C-c h a" . helm-apropos)
		 ("C-c h f" . helm-find-files)
		 ("C-c h o" . helm-occur)
		 ("C-c h m" . helm-man-woman))
  :config
  (require 'helm-command)
  (require 'helm-for-files)
  (require 'helm-imenu)
  (require 'helm-semantic)
  (require 'helm-misc)
  (setq helm-split-window-inside-p      t
		helm-M-x-fuzzy-match            t
		helm-buffers-fuzzy-matching     t
		helm-recentf-fuzzy-match        t
		helm-semantic-fuzzy-match       t
		helm-imenu-fuzzy-match          t
		helm-apropos-fuzzy-match        t
		helm-candidate-number-limit   100
		helm-autoresize-max-height     20
		helm-autoresize-min-height      0)
  (add-to-list 'helm-sources-using-default-as-input 'heml-source-man-pages)
  (helm-autoresize-mode t))

(use-package projectile
  :ensure t
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories
		'(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-file-suffixes
		'("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar"))
  (setq projectile-globally-ignored-files
		'("TAGS" "tags"))
  (with-eval-after-load "projectile"
	(projectile-mode)
	(helm-projectile-on)))

(use-package helm-projectile)

(use-package pdf-tools
  :pin manual
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package yasnippet
  :diminish yas-minor-mode
  :diminish yas-global-mode
  :bind (("C-c y r" . yas-reload-all)
		 ("C-c y n" . yas-new-snippet)
		 ("C-c y x" . yas-exit-snippet)
		 ("C-c y d" . yas-describe-tables)
		 ("C-c y v" . yas-visit-snippet-file)
		 ("C-c y l" . yas-load-snippet-buffer-and-close))
  :config
  (yas-global-mode 1))

(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :config
  (require 'js2-refactor)
  (define-key js2-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)
  (add-hook 'js2-mode-hook
			(lambda ()
			  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook
			(lambda ()
			  (setq mode-name "js2"))))

(use-package origami
  :bind (("C-<tab>" . origami-recursively-toggle-node)
		 ("C-c u"   . origami-open-all-nodes)
		 ("C-c f"   . origami-close-all-nodes)
		 ("C-c n"   . origami-show-only-node)
		 ("C-c r"   . origami-reset))
  :config
  (global-origami-mode t))

;; Split out auto-generated customizations
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Load Custom Libraries
;;(load-library "irc")
;;(load-library "fly-conf")
(load-library "keybindings")
(load-library "org-conf")
(load-library "elfeed-conf")

;; File Backup Management
(setq backup-by-copying t
      backup-directory-alist `(("." . "~/saves"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave File Management
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Add Abbrev-Mode Hooks
(dolist (hook '(erc-mode-hook
				emacs-lisp-mode-hook
				text-mode-hook
				org-mode-hook))
  (add-hook hook (lambda () (abbrev-mode 1))))
;; (setq default-abbrev-mode t) ;; Or, default on everywhere

;; Tramp Configuration
; TODO: if windows: use PuTTy Plink; if *nix: use ssh
(set-default 'tramp-auto-save-directory "~/.saves/tramp/")
(setq tramp-default-method "ssh")

;; Transparency?
(set-frame-parameter (selected-frame) 'alpha '(92 . 90))
(add-to-list 'default-frame-alist '(alpha . (92 . 90)))

;; Other Mode Customizations
(desktop-save-mode 1)
(setq auto-window-vscroll nil)
(setq frame-title-format "Emacs")
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)
(display-time-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(set-frame-font "monospace-11")
(setq epg-gpg-program "gpg2")

;; Auth
(setq auth-sources
	  '((:source "~/.emacs.d/secrets/.authinfo.gpg")))

;; Disable Trailing Whitespace highlight by Mode
(add-hook 'eshell-mode-hook
		  '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook
		  '(lambda () (setq show-trailing-whitespace nil)))

;; Magit/VC configuration
(setq vc-handled-backends (delq 'Git vc-handled-backends))

;; Set mode by File Ext. (generic)
(add-to-list 'auto-mode-alist '("\\.py$"            . python-mode))
(add-to-list 'auto-mode-alist '("\\.org$"           . org-mode))
(add-to-list 'auto-mode-alist '("\\.ts$"            . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.css$"           . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$"            . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$"      . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$"           . handlebars-mode))
(add-to-list 'auto-mode-alist '("\\.README\\.md$"   . gfm-mode))
(add-to-list 'auto-mode-alist '("Jenkinsfile$"      . groovy-mode))

;; Perl Configurations
(defalias 'perl-mode 'cperl-mode)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Override perl-mode with cperl-mode
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))

;;; HELPERS

;; Display which-function-mode, and do so in headerline instead of mode line
(which-function-mode)
(setq mode-line-misc-info (delete (assoc 'which-func-mode
					 mode-line-misc-info) mode-line-misc-info)
      which-func-header-line-format '(which-func-mode ("" which-func-format)))

(defadvice which-func-ff-hook (after header-line activate)
  (when which-func-mode
    (setq mode-line-misc-info (delete (assoc 'which-func-mode
					     mode-line-misc-info) mode-line-misc-info)
	  header-line-format which-func-header-line-format)))

;; Regardless of which buffer you're in, jump to the active minibuffer
(defun switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))

(defun occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[[:nonascii:]]"))

;;; END HELPERS

;; Enable flycheck globally:
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Customize flycheck
;; Disable clang check, gcc check works better
(setq-default flycheck-disabled-checkers
			  (append flycheck-disabled-checkers
					  '(c/c++-clang)))

;; Enable C++14 support for GCC
(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++14")))

(diminish 'auto-revert-mode)
(diminish 'visual-line-mode)
(diminish 'js2-refactor-mode)

;; Start the Emacs server
(require 'server)
(unless (server-running-p)
  (defvar server-name (concat "server"(number-to-string (emacs-pid))))
  (ignore-errors (server-start))

  ;; Set the environment variables for *shell*.
  (setenv "EDITOR" (concat "~/usr/local/bin/emacsclient -s " server-name)))

(provide 'init.el)
;;; init.el ends here
(put 'narrow-to-region 'disabled nil)
