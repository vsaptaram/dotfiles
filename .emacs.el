;; .emacs.el
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; run as server
(server-start)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; do not use custom-set-variables
(setq custom-file "~/.emacs-custom.el")
;; (load custom-file)  ;; not loading custom-file

;; remap Cmd/Opt for Apple keyboards
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'super
          mac-control-modifier 'control))

;; copy path variables from shell to emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; encoding
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; restart emacs
(use-package restart-emacs
  :ensure t)

;; appearances
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)

;; async mode

;; start up
(setq inhibit-splash-screen t
      initial-scratch-message nil)
      ;; initial-major-mode 'org-mode)

;; themes
(use-package spacemacs-common
    :ensure spacemacs-theme
    :init
    (load-theme 'spacemacs-dark t)
    (setq spacemacs-theme-org-agenda-height nil)
    (setq spacemacs-theme-org-height nil))


;; mode line
(use-package spaceline
  :ensure t
  ;; :defer t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; font
;; (set-frame-font "Fira Code 10")
;; (set-frame-font "Hack 10")
;; (set-frame-font "DejaVu Sans Mono-10" nil t)
(set-frame-font "Source Code Pro-11" nil t)
;; (set-frame-font "Cousine-10" nil t)

;; better defaults
(use-package better-defaults
  :ensure t)

;; window numbering
(use-package window-numbering
  :ensure t
  :init (window-numbering-mode 1))

;; windmove
(use-package windmove
  :ensure t
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   )
  :init
  (setq windmove-wrap-around t))

;; winner mode - restore window config
;; C-c-left/right
(use-package winner
  :ensure t
  :init
  (winner-mode))

;; crux
(use-package crux
  :ensure t
  :bind (("C-a" . crux-move-beginning-of-line)))

;; jump links
(use-package ace-link
    :init
    (ace-link-setup-default))

;; display class/function cursor is in
(which-function-mode)

;; misc
(setq tab-always-indent 'complete
      require-final-newline t
      column-number-mode t
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000
      global-prettify-symbols-mode +1
      x-stretch-cursor t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;; cursors
(blink-cursor-mode 0)
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

;; multi line edit
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)))

;; iedit - edit multiple occurs
(use-package iedit
  :bind ("C-;" . iedit-mode))

;; parenthesis
;; ;; autopair
;; (use-package autopair
;;   :ensure t
;;   :init
;;   (autopair-global-mode))

;; smartparens
(use-package smartparens
  :init
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-with-modes '(markdown-mode gfm-mode)
    (sp-local-pair "*" "*"))
  (sp-with-modes '(org-mode)
    ;; (sp-local-pair "=" "=")
    (sp-local-pair "*" "*")
    (sp-local-pair "/" "/")
    (sp-local-pair "_" "_")
    ;; (sp-local-pair "+" "+")
    (sp-local-pair "<" ">")
    (sp-local-pair "[" "]"))
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)))

;; company for auto complete
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; projectile
(use-package projectile
  :ensure t
  :after (helm helm-projectile)
  :bind (("C-c p". projectile-command-map))
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile))

;; helm
(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-h a" . helm-apropos))
  :init
  (helm-mode 1)
  (setq helm-M-x-fuzzy-match t))

(use-package helm-projectile
  :ensure t
  :init
  (helm-projectile-on))

;; perspective
(use-package perspective
  :ensure t
  ;; :commands persp-mode
  :init
  (persp-mode))

;; deft
(use-package deft
  :bind ("<f9>" . deft)
  :commands (deft)
  :config (setq deft-directory "/data/Dropbox/org_files/kb"
                deft-recursive t
                deft-default-extension "org"
                deft-extensions '("org")
                deft-text-mode 'org-mode
                deft-use-filter-string-for-filename t
                deft-file-naming-rules '((noslash . "-")
                                         (nospace . "-")
                                         (case-fn . downcase))))

;; magit
(use-package magit
  :ensure t
  :bind (("C-c ms" . magit-status)
         ("C-c ml" . magit-log-all))
  :custom
  (magit-log-arguments '("-n100" "--graph" "--decorate"))
  :config
  (use-package git-timemachine)
  (use-package git-link
    :init
    (setq git-link-open-in-browser t)))

;; neotree
(use-package neotree
  :ensure t
  :bind (("<f8>". neotree-toggle))
  :init
  (setq neo-smart-open t))

;; python environment
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setenv "WORKON_HOME" "~/virtual_envs")
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-virtualenv-path 'current)
  ;; (setq python-shell-interpreter "ipython"
  ;;            python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (setq elpy-rpc-timeout 10))

;; (use-package ein
;;   :ensure t)
;; (require 'ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)
;; ;; (setq ein:use-auto-complete t)
;; ;; (setq ein:complete-on-dot t)
;; (setq ein:completion-backend 'ein:use-company-backend)
;; ;; (setq ein:use-auto-complete-superpack nil)
;; (setq ein:notebook-modes '(ein:notebook-multilang-mode ein:notebook-python-mode ein:notebook-plain-mode))

(use-package flycheck
        :ensure t
        :init
        (global-flycheck-mode))
        ;; (when (require 'flycheck nil t)
        ;;      (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        ;;      (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package flyspell
  :bind (("C-`" . ispell-word)
         ("C-~" . ispell-buffer))
  :init
  ;; (dolist (hook '(text-mode-hook org-mode-hook))
  ;;   (add-hook hook (lambda () (flyspell-mode 1))))
  :config
  (setq ispell-program-name "aspell"
        ispell-list-command "--list"))

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; auto complete interface
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (ac-config-default))


;;; file reference registers
;; C-x r w a - where a is register name, to save window configuration
;; C-x r j a - to apply a window configuration
;; but it also stores files and not just pane configuration
;; checkout emacs-purpose - can integrate with perspective also
(set-register ?c (cons 'file "~/.emacs.el"))
(set-register ?i (cons 'file "/data/Dropbox/org_files/kb/gtd/inbox.org"))
(set-register ?p (cons 'file "/data/Dropbox/org_files/kb/gtd/projects.org"))
(set-register ?s (cons 'file "/data/Dropbox/org_files/kb/gtd/someday.org"))

;; org
(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda))
  :init
  (setq default-major-mode 'org-mode
        org-startup-indented t
        org-startup-truncated nil
        org-startup-with-inline-images t
        org-image-actual-width '(300)
        org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil
        org-cycle-separator-lines 2
        org-show-notification-handler 'message
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-src-fontify-natively t
        org-use-sub-superscripts (quote {})))

;; todo
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "RUNNING(r)" "ANALYSIS(a)"  "|" "DONE(d@)")
        (sequence "ALLOCATE(l/!)" "WAITING(w@/!)"  "|" "CANCELLED(c@/!)")))

;; effort estimates
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 1:00 2:00 3:00 4:00 6:00")))

(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")

;; tags
;; org-tag-alist '(("PROJECT" . ?p) ("MSD" . ?m) ("PERSONAL" . ?z) ("STUDY" . ?s) ("WRITING" . ?w) ("EMACS" . ?e))
(setq org-tag-alist '(
                      ;; Context / Areas
                      ("MSD" . ?M)
                      ("NEURO" . ?N)
                      ("PERSONAL" . ?V)
                      ("UNKNOWN" . ?U)
                      ;; type of work
                      ("READING" . ?r)
                      ("STUDY" . ?S)
                      ("WRITING" . ?w)
                      ("PROJECT" . ?P)
                      ;; topics
                      ("client" . ?c)
                      ("piano" . ?p)
                      ("emacs" . ?e)
                      ;; Time - or enforce through effort estimates
                      ("15min" . ?<)
                      ("<1h" . ?=)
                      (">>1h" . ?>)))

;; org bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org cliplink
(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

;; org rifle
(use-package helm-org-rifle
  :bind ("C-c o" . helm-org-rifle))

;; CAPTURE
(setq org-capture-templates
      '(("t" "Task" entry (file "/data/Dropbox/org_files/kb/gtd/inbox.org")
         "* TODO %?\n")))

;; AGENDAS
(setq org-agenda-inhibit-startup nil
      org-agenda-show-future-repeats t
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-window-setup 'current-window)


(setq org-agenda-files (file-expand-wildcards "/data/Dropbox/org_files/kb/gtd/*.org"))
;; remove someday.org
(setq org-agenda-files (remove "/data/Dropbox/org_files/kb/gtd/someday.org" org-agenda-files))

(setq vsr/org-agenda-directory "/data/Dropbox/org_files/kb/gtd/")

;; stuck projects
(setq org-stuck-projects
      '("+LEVEL=1+PROJECT/-DONE" ("NEXT") nil ""))

(setq org-agenda-custom-commands
      (quote (
              ("1" "Work Agenda"
               ((agenda "" nil)
                (todo "TODO"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-agenda-files '("/data/Dropbox/org_files/kb/gtd/inbox.org"))
                       (org-tags-match-list-sublevels 'indented)))
                (todo "RUNNING"
                      ((org-agenda-overriding-header "RUNNING Tasks")))
                (todo "ANALYSIS"
                      ((org-agenda-overriding-header "ANALYSIS Tasks")))
                (tags-todo "PROJECT+TODO=\"NEXT\""
                           ((org-agenda-overriding-header "NEXT Project Tasks")
                            (org-tags-match-list-sublevels 'indented)))
                (todo "ALLOCATE|WAITING"
                           ((org-agenda-overriding-header "WAITING Tasks")))
                (tags-todo "LEVEL=1+PROJECT"
                           ((org-agenda-overriding-header "Projects Overview")
                            (org-tags-match-list-sublevels 'nil)))
                (stuck ""
                       ((org-agenda-overriding-header "Stuck Projects")))
                (tags-todo "-PROJECT+TODO=\"NEXT\""
                           ((org-agenda-overriding-header "NEXT Standalone Tasks")))
                (tags-todo "+LEVEL>1/+PROJECT/+TODO"
                           ((org-agenda-overriding-header "Unscheduled TODO Project Tasks")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                )
               ((org-agenda-tag-filter-preset '("+MSD"))))
              ("2" "Personal Agenda"
               ((agenda "" nil)
                (todo "TODO"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-agenda-files '("/data/Dropbox/org_files/kb/gtd/inbox.org"))
                       (org-tags-match-list-sublevels 'indented)))
                (todo "RUNNING"
                           ((org-agenda-overriding-header "RUNNING Tasks")))
                (tags-todo "-PROJECT+TODO=\"NEXT\""
                           ((org-agenda-overriding-header "NEXT Standalone Tasks")))
                (tags-todo "LEVEL=1+PROJECT"
                           ((org-agenda-overriding-header "Projects Overview")
                            (org-tags-match-list-sublevels 'nil)))
                (stuck ""
                       ((org-agenda-overriding-header "Stuck Projects")))
                (tags-todo "PROJECT+TODO=\"NEXT\""
                           ((org-agenda-overriding-header "NEXT Project Tasks")
                            (org-tags-match-list-sublevels 'indented)))
                (tags-todo "+LEVEL>1/+PROJECT/+TODO"
                           ((org-agenda-overriding-header "Unscheduled TODO Project Tasks")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                )
               ((org-agenda-tag-filter-preset '("-MSD"))))
              )
             ))

;; refile
(setq vsr/kb_files (file-expand-wildcards "/data/Dropbox/org_files/kb/*.org"))
(setq org-refile-targets (quote (
                                 (nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9)
                                 (vsr/kb_files :maxlevel . 9)))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm)

;; clocking
(setq org-log-done 'time
      org-clock-idle-time nil
      org-clock-continuously nil
      org-clock-persist t
      org-clock-in-resume nil
      org-clock-report-include-clocking-task t
      org-clock-out-remove-zero-time-clocks t
      org-log-into-drawer t
      org-clock-into-drawer 1)

;; org habits
;; (add-to-list 'org-modules "org-habit")
;; (require 'org-habit)
;; (setq org-habit-graph-column 80)
;; (setq org-habit-show-habits-only-for-today nil)
;; (setq org-habit-preceding-days 14)
;; (setq org-habit-following-days 1)

;; org mobile staging area
;; (setq org-directory "~/Dropbox/org_files")
;; (setq org-mobile-inbox-for-pull "~/Dropbox/org_files/mobileorg.org")
;; (setq org-mobile-directory "~/Dropbox/")
;; (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; plant uml
(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  (setq org-plantuml-jar-path (expand-file-name "/home/vsaptaram/packages/plantuml.jar"))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; install pdf-tools
;; (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")
;; (use-package pdf-tools
;;   :ensure t
;;   :init
;;   (pdf-tools-install))

;; ;; auctex
;; (use-package tex
;;   :ensure auctex
;;   :init
;;   (setq TeX-source-correlate-method 'synctex
;;         TeX-view-program-selection '((output-pdf "PDF Tools"))
;;         TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;         TeX-source-correlate-start-server t) ;; not sure if neccessary
;;   (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))


;; helm-bibtex
(use-package helm-bibtex
  :ensure t
  :init
  (setq helm-bibtex-bibliography '("~/Dropbox/zotero/bibliography.bib")
        bibtex-completion-bibliography '("~/Dropbox/zotero/bibliography.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/Dropbox/org_files/research_notes/notes.org"))

;;; windows layout
(split-window-horizontally)
(split-window-horizontally)
(balance-windows)
