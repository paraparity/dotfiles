;;; org-conf.el --- byoung's Emacs org-mode customizations
;;
;;; Commentary:
;; Customizations for Emacs org-mode
;;
;;; Code:

(require 'org)
(require 'ob-ditaa)
(require 'ob-plantuml)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'org-journal)
(require 'org-tempo)
(require 'ox)
(require 'ox-asciidoc)
(require 'ox-pandoc)
(require 'ox-slimhtml)

(defvar org-directory "~/org")
(defvar org-default-notes-file (concat org-directory "/inbox.org"))

(setq org-modules
	  '((org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-tempo org-w3m)))


(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
		  '(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'org-mode-hook
		  '(lambda () (origami-mode nil)))

(setq org-clock-in-switch-to-state "STARTED")
(setq org-clock-report-include-clocking-task t)
(setq org-log-into-drawer "LOGBOOK")
(setq org-clock-into-drawer 1)
(setq org-log-done 'time)

(setq org-clock-continuously t)
(setq org-clock-in-resume t)
(setq org-expiry-inactive-timestamps t)
(setq org-src-window-setup 'current-window)

(org-clock-persistence-insinuate); Resume clocking task when emacs is restarted

;; Links - use like: cpan:HTML or rfc-txt:7522
(setq org-link-abbrev-alist
	  '(("rfc-txt" . "https://tools.ietf.org/rfc/rfc%s.txt")
		("cpan"    . "https://metacpan.org/search?q=%s")
		("ese"     . "https://emacs.stackexchange.com/search?q=%s")))

;; Agenda Files:
(setq org-agenda-files
	  '("~/org/agendas/organizer.org"
		"~/org/agendas/tickler.org"
		"~/org/agendas/waiting-on.org"
		;;"~/org/agendas/<person>.org"
		"~/org/agendas/review.org"
		"~/org/agendas/inbox.org"))

;; Org Todo Configuration:
(setq org-todo-keywords
	  '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@)" "|" "DONE(d)" "DROP(x@)")
		(sequence "REVIEW(r)"                          "|" "REVIEWED(v)")
		(sequence "MAYBE(m)"                           "|" "DROP(x@)")
		(sequence "OPEN(o)"                            "|" "CLOSED(c)")))

;; Faces Customization
(setq org-todo-keyword-faces
	  '(("STARTED" . "cyan")
		("MAYBE"   . "purple")
		("WAITING" . (:foreground "yellow" :weight bold))
		("DROP"    . "DimGray")))

(setq org-tag-alist
	  '(("@apartment"   . ?A)
		("@career"      . ?C)
		("@finances"    . ?F)
		("@guild"       . ?G)
		("@health"      . ?H)
		("@learn"       . ?L)
		("@network"     . ?N)
		("@office"      . ?O)
		("@read"        . ?R)
		("@self"        . ?S)
		("@travel"      . ?V)
		("@workstation" . ?W)
		("PROJECT"      . ?P)
		("TASK"         . ?T)
		("NEXT"         . ?X)
		("org-journal"  . ?j)
		))

(setq org-tags-exclude-from-inheritance '("PROJECT")
	  org-stuck-projects '("+PROJECT/-WAITING-DONE"
						   ("TODO" "STARTED") ()))

;; Org Agenda Custom Commands
;;; BEGIN HELPERS
(defun air/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
		(pri-value (* 1000 (- org-lowest-priority priority)))
		(pri-current (org-get-priority (thing-at-point 'line t))))
	(if (= pri-value pri-current)
		subtree-end
	  nil)))

(defun air/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
	(if (string= (org-entry-get nil "STYLE") "habit")
		subtree-end
	  nil)))
;;; END HELPERS


;; Org Agenda Conf
(setq org-agenda-skip-deadline-if-done t
	  org-agenda-skip-scheduled-if-done t)

;; (setq org-agenda-custom-commands
;; 	  ;; (1 key) (2 description (optional)) (3 type of search) (4 search term)
;; 	  '(("c" "Desk Work" tags-todo "computer"
;; 		 ((org-agenda-files '("~/org/widgets.org" "~/org/clients.org")) ;; (5 settings (optional))
;; 		  (org-agenda-sorting-strategy '(priority-up effort-down))) ;; (5 cont)
;; 		 ("~/computer.html")) ;; (6 export files (optional))
;; 		;; ... other commands
;; 		))
(setq org-agenda-custom-commands
	  '(("d" "Daily agenda and all TODOs"
		 ((tags "PRIORITY=\"A\""
				((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
				 (org-agenda-overriding-header "High-Priority Unfinished Tasks:")))
		  (agenda ""
				  ((org-agenda-span 1)))
		  (alltodo ""
				   ((org-agenda-skip-function
					 '(or (air/org-skip-subtree-if-habit)
						  (air/org-skip-subtree-if-priority ?A)
						  (org-agenda-skip-if nil '(scheduled deadline))))
					(org-agenda-overriding-header "All Normal Priority Tasks:"))))
		  ((org-agenda-compact-blocks t)))
		("D" "Daily Action List"
		 ((agenda ""
				  ((org-agenda-span 1)
				   (org-agenda-sorting-strategy
				   '((agenda time-up priority-down tag-up)))
				   (org-deadline-warning-ndays 0)))
		  ))
		("w" "Weekly Review"
		 ((agenda ""
				  ((org-agenda-span 7)))
		  (stuck "")
		  (tags "PROJECT")
		  (todo "WAITING")
		  ))
		("y" "Someday Maybe"
		 ((todo "MAYBE"
				((org-agenda-files '("~/org/maybe.org"))))
		  ))
		("r" "Review"
		 ((todo "REVIEW"
				((org-agenda-files '("~/org/review.org"))))
		  ))
		("X" "Upcoming Deadlines"
		 ((agenda ""
				  ((org-agenda-entry-types '(:deadline))
				   (org-agenda-span 1)
				   (org-deadline-warning-days 60)
				   (org-agenda-time-grid nil)))
		  ))
		("P" "Printed agenda"
		 ((agenda ""
				  ((org-agenda-span 7)
				   (org-agenda-start-on-weekday nil)
				   (org-agenda-repeating-timestamp-show-all t)
				   (org-agenda-entry-types '(:timestamp :sexp))))
		  (agenda ""
				  ((org-agenda-span 1)
				   (org-deadline-warning-days 7)
				   (org-agenda-todo-keyword-format "[ ]")
				   (org-agenda-scheduled-leaders '("" ""))
				   (org-agenda-prefix-format "%t%s")))
		  (todo "TODO"
				((org-agenda-prefix-format "[ ] %T: ")
				 (org-agenda-sorting-strategy '(tag-up prority-down))
				 (org-agenda-todo-keyword-format "")
				 (org-agenda-overriding-header "\nTasks by Context\n-----------------\n"))))
		 ((org-agenda-with-colors nil)
		  (org-agenda-compact-blocks t)
		  (org-agenda-remove-tags t)
		  (ps-number-of-columns 2)
		  (ps-landscape-mode t))
		 ("~/agenda.ps"))
		("Q" . "Custom Queries")
		("Qa" "Archive Search"
		 ((search ""
				  ((org-agenda-files
					(file-expand-wildcards "~/org/archive/*.org"))))
		  ))
		("QA" "Archive Tags Search"
		 ((org-tags-view ""
						 ((org-agenda-files
						   (file-expand-wildcards "~/org/archive/*.org"))))
		 ))
		))

;; Org Journal Configuration
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y%m%d")
(setq org-journal-date-format "%e %b %Y (%A)")
;;(setq org-journal-time-format "")

(defun get-journal-file-yesterday ()
  "Gets filename for yesterday's journal entry."
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
		 (daily-name (format-time-string "%Y%m%d" yesterday)))
	(expand-file-name (concat org-journal-dir daily-name))))

(defun journal-file-yesterday ()
  "Create and load a file based on yesterday's date."
  (interactive)
  (find-file (get-journal-file-yesterday)))

(defun org-journal-find-location ()
  "Open today's journal.
Specify a non-nil prefix to inhibit inserting the heading"
  (org-journal-new-entry t)
  (goto-char (point-min)))

;; Org Capture Configuration
(defvar org-capture-templates (list))

(add-to-list 'org-capture-templates
			 `("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
			   "\n* TODO %? :TASK:\n %i\n %a" :empty-lines 1))

(add-to-list 'org-capture-templates
			 `("p" "Project" entry (file+headline "~/org/inbox.org" "Projects")
			   "\n* TODO %? :PROJECT:\n %i\n %a" :empty-lines 1))

(add-to-list 'org-capture-templates
			 `("j" "Journal" entry (function org-journal-find-location)
			   "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))

(add-to-list 'org-capture-templates
			 `("n" "Note" entry (file+headline "~/org/refile.org" "Notes")
			   "* %?\n %i\n See: %a" :empty-lines 1))

(add-to-list 'org-capture-templates
			 `("T" "Tickler" entry (file+headline "~/org/tickler.org" "Tickler")
			   "\n* %i%? \n %U" :empty-lines 1))

(add-to-list 'org-capture-templates
			 `("c" "Item to Current Clock" item
			   (clock)
			   "%i%?" :empty-lines 1))

(add-to-list 'org-capture-templates
			 `("r" "Region to Current Clock" plain
			   (clock)
			   "%i" :immediate-finish t :empty-lines 1))


;;; BEGIN Capture Helpers
(defun region-to-clocked-task (start end)
  "Copies the selected text, from START to END, to the currently clocked in `org-mode` task."
  (interactive "r")
  (org-capture-string (buffer-substring-no-properties-start end) "C"))
(global-set-key (kbd "C-<F1>") 'region-to-clocked-task)

;; Meeting Focus
(defun meeting-notes ()
  "Call this after creating an `org-mode` heading for a meeting's notes.
After calling this function, call 'meeting-done' to reset the environment."
  (interactive)
  (outline-mark-subtree)
  (narrow-to-region (region-beginning) (region-ending))
  (deactivate-mark)
  (delete-other-windows)
  (text-scale-set 2)
  (fringe-mode 0)
  (message "When finished taking notes, run meeting-done"))

(defun meeting-done ()
  "Mostly restore the pre-meeting settings."
  (interactive)
  (widen)
  (text-scale-set 0)
  (fringe-mode 1)
  (winner-undo))
;;; END Capture Helpers

;; Org Refile Configuration
(setq org-refile-targets '((org-agenda-files :maxlevel . 6)))
;(setq org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Org Babel Configuration
(setq org-ditaa-jar-path "/usr/bin/ditaa.jar")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(setq org-src-fontify-natively t
	  org-src-tab-acts-natively t)

(defun bh/display-inline-images ()
  "Display inline images."
  (condition-case nil
	  (org-display-inline-images)
	(error nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C          . t)
   (ditaa      . t)
   (dot        . t)
   (emacs-lisp . t)
   (gnuplot    . t)
   (js         . t)
   (latex      . t)
   (ledger     . t)
   (org        . t)
   (perl       . t)
   (plantuml   . t)
   (python     . t)
   (sh         . t)
   (sql        . t)))

(add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))

(provide 'org-conf.el)
;;; org-conf.el ends here
