;;; fly-conf.el --- byoung's flycheck configuration
;;
;;; Commentary:
;; Customized on-the-fly checking in Emacs
;;
;;; Code:

(require 'flycheck)
(require 'projectile)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq flycheck-checkers
	  (quote (asciidoc
			  c/c++-clang
			  c/c++-cppcheck
			  css-csslint
			  emacs-lisp
			  emacs-lisp-checkdoc
			  handlebars
			  html-tidy
			  javascript-eslint
			  json-jsonlint
			  less
			  make
			  perl
			  perl-perlcritic
			  python-flak8
			  python-pylint
			  rust
			  sh-bash
			  sh-posix-bash
			  sh-zsh
			  sh-spellcheck
			  tex-chktex
			  tex-lacheck
			  texinfo
			  xml-xmlstarlet
			  xml-xmllint
			  yaml-jayaml
			  yaml-ruby)))
;; Others: cfenging chef-foodcritic coffee coffee-coffeelint d-dmd elixir
;;         erlang eruby-erubis go-gofmt go-golint go-vet go-build go-test
;;         haml haskell-ghc haskell-hlint lua php php-phpmd php-phpcs
;;         puppet-parser puppet-lint racket rst rst-sphinx ruby-rubocop
;;         ruby-rubylint ruby ruby-jruby sass scala scss slim verilog-verilator

(setq-default flycheck-disabled-checkers
			  (quote(javascript-jshint
					 javascript-jslint
					 javascript-gjslint)))

;; Use project relative eslint; see https://emacs.stackexchange.com/questions/21205
(defun my/use-eslint-from-node-modules ()
  "Use project local eslint node modules."
  (let* ((root (locate-dominating-file
				(or (buffer-file-name) default-directory)
				"node_modules"))
		 (eslint (and root
					  (expand-file-name "node_modules/eslint/bin/eslint.js"
										root))))
	(when (and eslint (file-executable-p eslint))
	  (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; See: https://emacs.stackexchange.com/questions/13065
;; (defun setup-flycheck-clang-project-path ()
;;   "Use project local clang."
;;   (let ((root (ignore-errors (projectile-project-root))))
;; 	(when root
;; 	  (add-to-list
;; 	   (make-variable-buffer-local 'flycheck-clang-include-path)
;; 	   root))))

;; (add-hook 'c++-mode-hook 'setup-flycheck-clang-project-path)

(provide 'fly-conf.el)
;;; fly-conf.el ends here
