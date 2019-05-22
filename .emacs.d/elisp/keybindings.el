;;; keybindings.el --- byoung's custom Emacs keybindings
;;
;;; Commentary:
;; Custom Key Bindings for Emacs
;;
;;; Code:

;; Buffer Key Bindings
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-x |") 'window-toggle-split-direction)
(global-set-key (kbd "C-c o") 'switch-to-minibuffer)
(global-set-key (kbd "<f8>") 'flyspell-buffer)
(global-set-key (kbd "<f6>") 'speedbar)

;; Org-Mode Keybindings
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

;; Elfeed Keybindings
(global-set-key (kbd "C-x w") 'elfeed)

;; Misc Keybindings
(global-set-key (kbd "<f5>") 'delete-trailing-whitespace)
(global-set-key (kbd "C-x #") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;;(global-set-key (kbd "M-.") 'find-tag-other-window)

;; Numpad Key Bindings
(global-set-key "\eOp" "0")
(global-set-key "\eOq" "1")
(global-set-key "\eOr" "2")
(global-set-key "\eOs" "3")
(global-set-key "\eOt" "4")
(global-set-key "\eOu" "5")
(global-set-key "\eOv" "6")
(global-set-key "\eOw" "7")
(global-set-key "\eOx" "8")
(global-set-key "\eOy" "9")
(global-set-key "\eOl" "+")
(global-set-key "\eOn" ".")

;; Modal Key Bindings
(add-hook 'cperl-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-h f") 'cperl-perldoc)))

;; Kill the bane of my fat-fingers
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(provide 'keybindings.el)
;;; keybindings.el ends here
