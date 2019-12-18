;; init.el
;; init scripts

;;; load package archives
(load "package")
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; auto install packages
;; (package-refresh-contents)
(dolist (package '(use-package))
   (unless (package-installed-p package)
     (package-install package)))

;;; because of an elpa issue
; from here - https://irreal.org/blog/?p=8243
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; copy path variables from shell to emacs
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;;; start up
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;;; appearances
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; remap Cmd/Opt for Apple keyboards
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
	  mac-option-modifier 'super
	  mac-control-modifier 'control))

;;; system clipboard
(setq x-select-enable-clipboard t)

;;; editor
(setq-default indicate-empty-lines t)

(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 4
      indent-tabs-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent)
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

;;; set font
;; (set-frame-font "Fira Code 10")
;; (set-frame-font "DejaVu Sans Mono-10" nil t)
(set-frame-font "Source Code Pro-10" nil t)
;; (set-frame-font "Cousine-10" nil t)

; display class/function cursor is in
;; (which-function-mode)

;;; show matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

(setq echo-keystrokes 0.1
      use-dialog-box nil)

;;; load lisp scripts
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;;; dont backup files
(setq make-backup-files nil)

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

;;; themes
(use-package spacemacs-common
    :ensure spacemacs-theme
    :init (load-theme 'spacemacs-dark t))

;; (use-package spacemacs-theme
;;   :ensure t
;;   :init
;;   (load-theme 'spacemacs-dark t))
;; (load-theme 'spacemacs-dark t)

;; (load-theme 'flatland t)

;;; mode line
;; (package-refresh-contents)
(use-package spaceline
  :ensure t
  ;; :defer t
  :init
  (spaceline-emacs-theme))
;; (require 'spaceline-config)
;; (spaceline-spacemacs-theme)

;;; Windows
(use-package window-numbering
  :ensure t
  :init (window-numbering-mode 1))
;; (window-numbering-mode 1)

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

;;; cursors
(blink-cursor-mode 0)
(use-package beacon
  :ensure t
  :init (beacon-mode 1))

; multi line edit
(use-package multiple-cursors
  :ensure t
  :bind (("C-c m c" . mc/edit-lines)))

;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-c m c") 'mc/edit-lines)


;;; autopair
(use-package autopair
  :ensure t
  :init
  (autopair-global-mode))
;; (require 'autopair)

;;; auto complete
;; (require 'auto-complete-config)
;; (ac-config-default)e

;;; eshell

;; shell switcher
;; (require 'shell-switcher)
;; (use-package shell-switcher
;;   :ensure t
;;   :init
;;   (setq shell-switcher-mode t)
;;   :hook (eshell-mode-hook . shell-switcher-manually-register-shell))

;; virtualenvwrapper
;; (require 'virtualenvwrapper)
;; (venv-initialize-eshell)
;; (setq venv-location "~/Dropbox/virtual_envs")

;;; projectile
(use-package projectile
  :ensure t
  :after (helm helm-projectile)
  :bind (("C-c p". projectile-command-map))
  :init
  (projectile-global-mode)
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

;; (require 'helm)
;; (require 'helm-config)
;; (setq helm-M-x-fuzzy-match t)
;; (helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-h a") 'helm-apropos)
;;(global-set-key (kbd "C-c C-j") 'helm-imenu)

;;; projectile
;; (projectile-global-mode)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;; (helm-projectile-on)
;; (setq projectile-completion-system 'helm)
;; (setq projectile-switch-project-action 'helm-projectile)
;; (setq projectile-project-search-path '("~/code/"))

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
;; (global-set-key (kbd "C-c ms") 'magit-status)
;; (global-set-key (kbd "C-c ml") 'magit-log-all)

;;; neotree
(use-package neotree
  :ensure t
  :bind (("<f8>". neotree-toggle))
  :init
  (setq neo-smart-open t))

;; (require 'neotree)
;; (global-set-key [f8] 'neotree-toggle)
;; (setq neo-smart-open t)

;;; company autocomplete
;; (use-package company               
;;   :ensure t
;;   :defer t
;;   :init
;;   (global-company-mode)
;;   :config
;;   (progn
;;     ;; Use Company for completion
;;     (bind-key [remap completion-at-point] #'company-complete company-mode-map)

;;     (setq company-show-numbers t)
;;      ;; company-tooltip-align-annotations t
;;           ;; Easy navigation to candidates with M-<n>
          
;;     ;; (setq company-dabbrev-downcase nil))
;;   :diminish company-mode)

;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode))

;;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setenv "WORKON_HOME" "~/Dropbox/virtualenvs")
  (setq elpy-rpc-python-command "python3"
	elpy-rpc-virtualenv-path 'current)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"))
  ;; (setq python-shell-interpreter "jupyter"
  ;;     python-shell-interpreter-args "console --simple-prompt"
  ;;     python-shell-prompt-detect-failure-warning nil)
  ;; (add-to-list 'python-shell-completion-native-disabled-interpreters
  ;; 	       "jupyter"))

;; config from http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;   :bind (:map elpy-mode-map
;; 	      ("<M-left>" . nil)
;; 	      ("<M-right>" . nil)
;; 	      ("<M-S-left>" . elpy-nav-indent-shift-left)
;; 	      ("<M-S-right>" . elpy-nav-indent-shift-right)
;; 	      ("M-." . elpy-goto-definition)
;; 	      ("M-," . pop-tag-mark))
;;   :init
;;   (setq elpy-rpc-backend "jedi"
;; 	elpy-rpc-virtualenv-path 'current
;;         elpy-rpc-python-command "python3")
;;   (setenv "WORKON_HOME" "~/Dropbox/virtualenvs"))

;; (setq python-shell-interpreter "jupyter")

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;; 	       "jupyter")

;; (use-package python
;;   :ensure t
;;   :mode ("\\.py" . python-mode)
;;   :init
;;   (setq python-indent-offset 4)
;;   (elpy-enable))

;;; uninstalling pyenv
;; (use-package pyenv-mode
;;   :ensure t
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x p e" . pyenv-activate-current-project))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
;;     (if python-version-directory
;;         (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
;;                (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;           (pyenv-mode-set pyenv-current-version)
;;           (message (concat "Setting virtualenv to " pyenv-current-version))))))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;             (let ((pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;               (pyenv-mode-set pyenv-current-version)
;;               (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; ;; set global mode
;; (defvar pyenv-current-version nil nil)

;; (defun pyenv-init()
;;   "Initialize pyenv's current version to the global one."
;;   (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
;;     (message (concat "Setting pyenv version to " global-pyenv))
;;     (pyenv-mode-set global-pyenv)
;;     (setq pyenv-current-version global-pyenv)))

;; (add-hook 'after-init-hook 'pyenv-init)

;; (use-package load-relative
;;   :ensure t)

;; (use-package loc-changes
;;   :ensure t)

;; (use-package realgud
;;   :ensure realgud)


;; (elpy-enable)
;; ;; (setq elpy-rpc-backend "jedi")
;; (require 'pyvenv)
;; ;; (pyvenv-activate "~/Dropbox/virtual_envs/emacs-python3/")
;; ;; (pyvenv-activate "~/Dropbox/virtual_envs/emacs-python3-ubuntu/")
;; (pyvenv-activate "/home/vsaptaram/Dropbox/virtual_envs/p3ml")
;; (global-set-key (kbd "M-.") 'elpy-goto-definition-other-window)

;;; ein
;; (require 'ein-notebook)
;; (use-package ein
;;   :defer t
;;   :commands ein:notebooklist-open
;;   :init
;;   (progn
;;     (with-eval-after-load 'ein-notebooklist
;;       ;; removing keybindings
;;       ;; (define-key ein:notebook-mode-map (kbd "M-p") nil)
;;       ;; (define-key ein:notebook-mode-map (kbd "M-n") nil)
;;       ;; (define-key ein:notebook-mode-map (kbd "<M-up>") nil)
;;       ;; (define-key ein:notebook-mode-map (kbd "<M-down>") nil)
;;       ;; changing keybinding
;;       (define-key ein:notebook-mode-map (kbd "<C-tab>") 'ein:completer-complete)
;;       (define-key ein:notebook-mode-map (kbd "<M-S-up>") 'ein:worksheet-move-cell-up)
;;       (define-key ein:notebook-mode-map (kbd "<M-S-down>") 'ein:worksheet-move-cell-down))))

(require 'ein-notebook)
(use-package ein
  :ensure t
  :init
  (setq ein:use-auto-complete t
	ein:console-args '("--simple-prompt")))

;; (persp-mode)
;; (require 'persp-projectile)
;;; not working

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

;; (setq org-agenda-files
;;       (file-expand-wildcards "~/Dropbox/org_files/*.org"))
;; (setq org-agenda-files '("~/Dropbox/org_files/projects.org"
;;                          "~/Dropbox/org_files/in.org"
;;                          "~/Dropbox/org_files/msd.org"))

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

;; org agenda customize variables
(setq org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-agenda-span 7)

;; (require 'magic-latex-buffer)

;; org custom agenda
;; (setq org-agenda-custom-commands
;;       '(("x" agenda)
;;         ("y" agenda*)))

;; (defun zin/org-agenda-skip-tag (tag &optional others)
;;   "Skip all entries that correspond to TAG.
;; If OTHERS is true, skip all entries that do not correspond to TAG."
;;   (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
;;         (current-headline (or (and (org-at-heading-p)
;;                                    (point))
;;                               (save-excursion (org-back-to-heading)))))
;;     (if others
;;         (if (not (member tag (org-get-tags-at current-headline)))
;;             next-headline
;;           nil)
;;       (if (member tag (org-get-tags-at current-headline))
;;           next-headline
;;         nil))))

;; (setq org-agenda-custom-commands
;;       '(("w" "MSD Agenda"
;;          ((agenda ""
;; 		  ((org-agenda-skip-function '(zin/org-agenda-skip-tag "MSD" 't))))
;;           (tags-todo "MSD")))
;; 	("z" "Personal Agenda"
;;          ((agenda ""
;; 		  ((org-agenda-skip-function '(zin/org-agenda-skip-tag "PERSONAL" 't))))
;;           (tags-todo "PERSONAL")))))

;; org - google calendar integration
;; org-gcal
;; (require 'org-gcal)
;; (setq org-gcal-client-id "271720850051-g7ujaohop0c0dp4ej5s8sc4ce0pto67g.apps.googleusercontent.com"
;;       org-gcal-client-secret "rbCRji710Pyj-t0yFUZLLoxk"
;;       org-gcal-file-alist '(("vignesh@madstreetden.com" .  "~/Dropbox/org_files/msd-calendar.org")))


;; org-caldav setup
;; (require 'org-caldav)
;; (setq org-caldav-url 'google
;;       org-caldav-calendar-id "vignesh@madstreetden.com"
;;       org-caldav-inbox "~/Dropbox/org_files/msd-calendar.org"
;;       org-caldav-oauth2-client-id "271720850051-g7ujaohop0c0dp4ej5s8sc4ce0pto67g.apps.googleusercontent.com"
;;       org-caldav-oauth2-client-secret "rbCRji710Pyj-t0yFUZLLoxk"
;;       )

;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epg-gpg-program "/usr/local/MacGPG2/bin/gpg2")

;; (require 'epa-file)
;; (custom-set-variables '(epg-gpg-program  "/usr/local/bin/gpg"))
;; (epa-file-enable)

;;; plant uml
;; Enable plantuml-mode for PlantUML files
(use-package plantuml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
               ;; 'org-src-lang-modes '("plantuml" . plantuml)))

;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;; (add-to-list
;;   'org-src-lang-modes '("plantuml" . plantuml))

;;; PDF
;; latex
;; (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

;; install pdf-tools
;; (setenv "PKG_CONFIG_PATH" "/usr/local/lib/pkgconfig:/usr/local/Cellar/libffi/3.2.1/lib/pkgconfig")
(use-package pdf-tools
  :ensure t
  :init
  (pdf-tools-install))

;; (pdf-tools-install)

;; auctex
(use-package tex
  :ensure auctex
  :init
  (setq TeX-source-correlate-method 'synctex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t) ;; not sure if neccessary
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

;; to use pdf-tools with auctex
;; (setq TeX-source-correlate-method 'synctex)
;; (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
;;     TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
;;     TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
;; ;; (setq TeX-view-program-list '(("PDF Tools" "TeX-pdf-tools-sync-view")))

;; ;; to have the buffer refresh after compilation
;; (add-hook 'TeX-after-compilation-finished-functions
;; 	  #'TeX-revert-document-buffer)

;; helm-bibtex
(use-package helm-bibtex
  :ensure t
  :init
  (setq helm-bibtex-bibliography '("~/Dropbox/zotero/bibliography.bib")
	bibtex-completion-bibliography '("~/Dropbox/zotero/bibliography.bib")
	bibtex-completion-pdf-field "file"
	bibtex-completion-notes-path "~/Dropbox/org_files/research_notes/notes.org"))

;; (setq reftex-default-bibliography '("~/Dropbox/zotero-other_files/biblio.bib") )
;; (setq helm-bibtex-bibliography '("~/Dropbox/zotero-other_files/biblio.bib"))
;; (setq bibtex-completion-bibliography
;;       '("~/Dropbox/zotero-other_files/biblio.bib"))

;; open pdf file
;; (setq bibtex-completion-pdf-field "file")
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;     (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath)))

;; notes using org-noter
;; (setq org-noter-default-notes-file-names '("notes.org")
;;       org-noter-notes-search-path '("~/Dropbox/org_files/research_notes"))
;; (setq org-noter-separate-notes-from-heading t)

;; Activate zotxt
;; (add-hook 'org-mode-hook (lambda () (org-zotxt-mode 1)))

;;; wakatime for tracking
;; (global-wakatime-mode)

;;; TIPS and things to remember
;; C-x 4 c: open the same file in different buffer

;;; windows layout
(split-window-horizontally)
(split-window-horizontally)
(balance-windows)

;; open init file on load
;; (find-file "~/.emacs.d/init.el")

;;; end configurations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (realgud-ipdb realgud helm-projectile window-numbering use-package spacemacs-theme spaceline projectile pdf-tools neotree multiple-cursors magit helm-org-rifle beacon autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
