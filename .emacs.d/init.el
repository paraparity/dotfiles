;;; init.el --- Load paraparity's literate configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Bootstraps package and ensures use-package before loading my literate configuration.
;; Contains a few initialization optimizations.

;;; Code:
(setq debug-on-error t)

;; I have often seen and considerd using gc-cons-threshold init optimizations, but haven't fully understood the why
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
;; For now, lets just take a measurement to see if this warrants my attention
;; Per: https://blog.d46.us/advanced-emacs-startup/
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

;; Load from literate configuration
(org-babel-load-file "~/.emacs.d/configuration.org")
