(require 'package)

;; Adds the Melpa archive to the list of available repositories

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;; appearances
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; start up
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;;; themes
(use-package spacemacs-common
    :ensure spacemacs-theme
    :init (load-theme 'spacemacs-dark t))

;;; mode line
(use-package spaceline
  :ensure t
  ;; :defer t
  :init
  (spaceline-emacs-theme))

;;; remap Cmd/Opt for Apple keyboards
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'super
	  mac-control-modifier 'control))

;; better defaults
(use-package better-defaults
	:ensure t)


;;; Windows
(use-package window-numbering
  :ensure t
  :init (window-numbering-mode 1))

;; windmove
(use-package windmove
  ;; :defer 4
  :ensure t
  :init
  ;; TODO: use command key on Mac
  (windmove-default-keybindings 'shift)
  ;; wrap around at edges
  (setq windmove-wrap-around t))

;; winner mode to restore window config
(use-package winner
  :ensure t
  :init
  (winner-mode))

;;; copy path variables from shell to emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;; set font
;; (set-frame-font "Fira Code 10")
;; (set-frame-font "DejaVu Sans Mono-10" nil t)
(set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Cousine-10" nil t)

; display class/function cursor is in
;; (which-function-mode)

;;; misc
(setq tab-always-indent 'complete
      require-final-newline t
      column-number-mode t
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000)


;;; file reference registers
;; C-x r w a - where a is register name, to save window configuration
;; C-x r j a - to apply a window configuration
;; but it also stores files and not just pane configuration
;; checkout emacs-purpose - can integrate with perspective also
(set-register ?c (cons 'file "~/.emacs.el"))
(set-register ?i (cons 'file "~/Dropbox/org_files/in.org"))
(set-register ?r (cons 'file "~/Dropbox/org_files/repository.org"))
(set-register ?p (cons 'file "~/Dropbox/org_files/projects.org"))
(set-register ?s (cons 'file "~/Dropbox/org_files/someday.org"))
(set-register ?m (cons 'file "~/Dropbox/org_files/mobileorg.org"))
(set-register ?w (cons 'file "~/Dropbox/org_files/msd.org"))

;;; cursors
(blink-cursor-mode 0)
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

; multi line edit
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)))

 ;;; autopair
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode))


;;; projectile
(use-package projectile
  :ensure t
  :after (helm helm-projectile)
  :bind (("C-c p". projectile-command-map))
  :init
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile))

;;; helm
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

;;; perspective
(use-package perspective
  :ensure t
  ;; :commands persp-mode
  :init
  (persp-mode))

;;; magit
(use-package magit
  :ensure t
  :bind (("C-c ms" . magit-status)
         ("C-c ml" . magit-log-all)))

;;; neotree
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
  (setenv "WORKON_HOME" "~/Dropbox/virtualenvs")
  (setq elpy-rpc-python-command "python3"
        elpy-rpc-virtualenv-path 'current)
  ;; (setq python-shell-interpreter "ipython"
  ;;      	python-shell-interpreter-args "-i --simple-prompt")
  (setq python-shell-interpreter "jupyter"
        python-shell-interpreter-args "console --simple-prompt"
        python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))

;; (use-package ein
;; 	:ensure t
;; 	:init)

;; (require 'ein)
;; (require 'ein-notebook)
;; (require 'ein-subpackages)

(use-package flycheck
	:ensure t
	:init
        (global-flycheck-mode))
	;; (when (require 'flycheck nil t)
	;; 	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	;; 	(add-hook 'elpy-mode-hook 'flycheck-mode)))

;;; auto complete interface
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (ac-config-default))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;; org
;;; org mode
(require 'org)
(require 'org-inlinetask)

(setq org-goto-interface 'outline-path-completion)
(setq org-log-done t
      org-cycle-separator-lines 2)
(with-eval-after-load 'org
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode))

(global-set-key (kbd "C-c o h") 'helm-org-in-buffer-headings)

(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file "~/Dropbox/org_files/in.org")

;; (use-package helm-org-rifle
;;   :ensure t
;;   :init
;;   (setq helm-org-rifle-show-path t))

;; (require 'helm-org-rifle)
;; (setq helm-org-rifle-show-path t)

;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

; org agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-agenda-show-log t
      org-agenda-window-setup 'current-window)

(setq org-agenda-files (list "~/Dropbox/org_files/projects.org"
                             "~/Dropbox/org_files/in.org" 
                             "~/Dropbox/org_files/msd.org"))

; refile targets
;; (setq org-refile-targets '((nil :maxlevel . 5)
;;                            (org-agenda-files :maxlevel . 5)))
(setq org-refile-targets '(("~/Dropbox/org_files/projects.org" :maxlevel . 3)
                           ("~/Dropbox/org_files/repository.org" :level . 3)
                           ("~/Dropbox/org_files/someday.org" :level . 3)
                           ("~/Dropbox/org_files/msd.org" :maxlevel . 5)))

; refile with filenames/path instead of being flat
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
; refile create parents on the fly
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Todo keywords
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@)" "APPT(a)"  "|" "DONE(d@)" "DEFERRED(f@)" "CANCELLED(c@)")))
;; (require 'org-inlinetask)
;; (setq org-log-done 'note)
(setq org-log-into-drawer t)

;; org tags and properties
(setq org-tag-alist '(("PROJECT" . ?p) ("MSD" . ?m) ("PERSONAL" . ?z) ("STUDY" . ?s) ("WRITING" . ?w) ("EMACS" . ?e)))

; global Effort estimate values
(setq org-global-properties
      '(("Effort_ALL" .
         "0:15 0:30 1:00 1:30 2:00 3:00 4:00 5:00 6:00 0:00")))
(setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM")


;; org mobile staging area
(setq org-directory "~/Dropbox/org_files")
(setq org-mobile-inbox-for-pull "~/Dropbox/org_files/mobileorg.org")
(setq org-mobile-directory "~/Dropbox/")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;; org agenda customize variables
(setq org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-agenda-span 7)

;; org habit tracking
; (add-to-list 'org-modules "org-habit")
; (require 'org-habit)
; (setq org-habit-graph-column 80)
; (setq org-habit-show-habits-only-for-today nil)
; (setq org-habit-preceding-days 42)
; (setq org-habit-following-days 1)

;; org mobile staging area
(setq org-directory "~/Dropbox/org_files")
(setq org-mobile-inbox-for-pull "~/Dropbox/org_files/mobileorg.org")
(setq org-mobile-directory "~/Dropbox/")
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


;;; plant uml
;; Enable plantuml-mode for PlantUML files
(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
               ;; 'org-src-lang-modes '("plantuml" . plantuml)))


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
