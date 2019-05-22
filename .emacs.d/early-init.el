;;; package --- Summary:
;;; early-init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold 268435456)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; One less file to load at startup
(setq site-run-file nil)
(provide 'early-init)
;;; early-init.el ends here
