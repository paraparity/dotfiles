;;; init.el --- paraparity's Emacs configuration file
;;
;;; Commentary:
;; Loads my literate configuration
;;
;;; Code:
(package-initialize)

;(setq debug-on-error t)

(org-babel-load-file "~/.emacs.d/configuration.org")
