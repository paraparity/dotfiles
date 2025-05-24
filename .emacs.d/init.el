;;; init.el --- Load paraparity's literate configuration -*- lexical-binding: t -*-
;;; Commentary:

;; Bootstraps package and ensures use-package before loading my literate configuration.
;; Contains a few initialization optimizations.

;;; Code:
;;(setq debug-on-error t)

(if init-file-debug
    (setq debug-on-error t))

;; Restore garbage collection settings after startup
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold 100000000
                  gc-cons-percentage 0.2)
	    (message "gc-cons-threshold restored to %s and gc-cons-percentage to %s"
                     gc-cons-threshold
                     gc-cons-percentage)))

;; Per: https://blog.d46.us/advanced-emacs-startup/
;; Let's monitor startup performance
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; Configure package.el to include melpa
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("gnu"    . "https://elpa.gnu.org/packages/")
			   ("melpa"  . "https://melpa.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize))

;; Ensure desired packages are installed, cache package refresh contents
(unless	(and (package-installed-p 'use-package)
	     (package-installed-p 'org-contrib))
  (package-refresh-contents)

  (unless (package-installed-p 'use-package)
    (package-install 'use-package)
    (package-install 'use-package-ensure-system-package))

  (unless (package-installed-p 'org-contrib)
    (package-install 'org-contrib)))

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-always-defer t)

(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; Trying to load this early as it's having trouble in my configuration.org
(use-package org-ql
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :if (or (memq window-system '(mac ns x))
          (daemonp))
  :config
  (exec-path-from-shell-initialize))

;; Broken in [feature/native-comp]
;; Ref:
;; - https://github.com/dholm/benchmark-init-el/issues/15
;; - https://github.com/hlissner/doom-emacs/issues/4534
;;
;; Since we're benchmarking. Let's see where I can do better
;;(use-package benchmark-init
;;  :ensure t
;;  :config
;;  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Conditionally enable native-comp
(if (and (fboundp 'native-comp-available-p)
	 (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t
          native-comp-async-report-warnings-errors 'silent
          byte-compile-warnings '(not docstrings free-vars lexical))
  (message "Native compilation is *not* available"))

(add-hook 'after-init-hook
	  (lambda () (message "loaded in %s" (emacs-init-time))))

;; Load from literate configuration
(org-babel-load-file "~/.emacs.d/configuration.org")

(provide 'init)
;;; init.el ends here
