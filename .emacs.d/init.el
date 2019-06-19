;;; init.el --- Load paraparity's literate configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Bootstraps package and ensures use-package before loading my literate configuration.
;; Contains a few initialization optimizations.

;;; Code:
;;(setq debug-on-error t)

;; Since Emacs 27 hasn't yet been released, let's duplicate my gc code from my preemptive early-init.el
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold 268435456)

;; Restore after startup
(add-hook 'after-init-hook
		  (lambda ()
			(setq gc-cons-threshold 1000000)
			(message "gc-cons-threshold restored to %s" gc-cons-threshold)))

;; Per: https://blog.d46.us/advanced-emacs-startup/
;; Let's monitor startup performance
(add-hook 'emacs-startup-hook
		  (lambda ()
			(message "Emacs ready in %s with %d garbage collections."
					 (format "%.2f seconds"
							 (float-time (time-subtract after-init-time before-init-time)))
					 gcs-done)))

;; Configure package.el to include melpa
(require 'package)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
						 ("gnu"   . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Ensure that use-package is installed.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Since we're benchmarking. Let's see where I can do better
(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
		  (lambda () (message "loaded in %s" (emacs-init-time))))

;; Load from literate configuration
(org-babel-load-file "~/.emacs.d/configuration.org")
