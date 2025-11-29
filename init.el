;; -> Start initialization

;; -> If EMACS run in GUI mode / has these functions
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))          ;; Disable bar icon on top

;; -> If EMACS run in GUI mode / has these functions
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))        ;; Disable scrollbar

;; -> Hide menu on top
(menu-bar-mode -1)

;; -> Enable tab's on top
(tab-bar-mode 1)

;; -> Switch cursor to bar-view in GUI mode
(when (display-graphic-p (selected-frame))
  (setopt cursor-type 'bar))

;; -> Enable insert TAB-key or indent region with TAB-key
(defun my-tab ()
  "If region is active – indent it, otherwise insert a TAB char."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (insert "\t"))) 			;; Really TAB
(keymap-global-set "TAB" #'my-tab)
(setq-default tab-width 4)

;; -> Enable IDO-mode to all buffers
(ido-mode 'both)

;; -> Enable mode of list recent files
(use-package recentf
  :custom
  (recentf-max-saved-items 100 "Remember last 100 files")
  (recentf-save-file (locate-user-emacs-file "recentf") "Sace file list in file .emacs.d/recentf")
  :config (recentf-mode t))
(keymap-global-set "C-x C-g" 'recentf-open-files)	;; Set keybindings for open menu recent files

;; -> Enable restore cursor position in file
(use-package saveplace
  :custom
  (save-place-forget-unreadable-files t "Don't remember position in not read files.")
  :config
  (save-place-mode t))

;; -> Remember command history
(use-package savehist
  :hook
  (server-done . savehist-save)
  (kill-emacs . savehist-save)
  :config
  (add-to-list 'delete-frame-functions 'savehist-save)
  (savehist-mode t))

;; -> Enable delete marked text with typing or DEL, BACKSPACE
(delete-selection-mode 1)

;; -> Set some variables
(setq use-short-answers t							;; Use short y and n for answers
	  inhibit-splash-screen t						;; Disable start screen EMACS
	  initial-scratch-message nil					;; Disable message on top SCRATCH buffer
	  use-file-dialog nil							;; Ask for textual confirmation instead of GUI
	  make-backup-files nil							;; Don't make backup file
	  auto-save-list-file-name nil					;; Don't make autosave file list
	  auto-save-default nil							;; Disable default autosave files buffer
	  ring-bell-function 'ignore					;; Shut up the bell
	  dired-kill-when-opening-new-dired-buffer t)	;; Delete buffer when goto next directory

;; -> Enable smooth scroll buffer
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; -> Define function for align all comments in region
(defun align-comments ()
  "Align all comments in region by comment char."
  (interactive)															
  (align-regexp (region-beginning) (region-end)							
                (concat "\\(\\s-*\\)" (regexp-quote comment-start))))	
(keymap-global-set "C-c a c" 'align-comments)		;; Set keybindings for align-comments

;;-> Set priorities of archives
(setq package-archive-priorities
      '(("gnu" . 40)
        ("nongnu" . 30)
        ("melpa-stable" . 20)
        ("melpa" . 10))
 	  package-native-compile t) ;; "Compile packages from install but not first start"

;; Packages initialization
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; -> Refresh packages list if it not present
(unless package-archive-contents
  (message "Refresh list archives")
  (package-refresh-contents))						

;;(unless (package-installed-p 'use-package)			;; Uncomment this if needed oldest version EMACS <28
;;	(package-install 'use-package))

;; -> Enable to use 'use-package package
(require 'use-package)
;; -> Always check present package befor install
(setq use-package-always-ensure t)

;; -> A dark theme with contrasting colours
(use-package abyss-theme
  :defer t)

;; Install needed packages
(dolist (package-need-install '(magit racket-mode
								slime tramp vterm
								xterm-color
								cider clojure-mode clojure-snippets
								helm-cider paredit))
  (eval `(use-package ,package-need-install)))

;; -> Builtin package. Save and restore EMACS state between session
(use-package desktop
  :custom
  (desktop-dirname user-emacs-directory "Directory for saved file .desktop")
  (desktop-auto-save-timeout 20 "Autosave every 20 second")
  (desktop-load-locked-desktop t "Load desktop-file but if it blocked")
  (desktop-restore-frames t "Restore frame state")
  (desktop-save t "Save state desktop without questions")
  :config
  ;; Modes buffersnot need save and restore
  (add-to-list 'delete-frame-functions 'desktop-save)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  ;; Switch mode to enable
  (desktop-save-mode 1)
  :hook
  (after-init . desktop-read)
  (server-after-make-frame . desktop-read)
  (kill-emacs . (lambda () (desktop-save user-emacs-directory t)))
  (server-done . desktop-save))

;; -> ELEC-PAIR
;; Builtin packages
;; Automatic insert pair symbols.
;; If select region then pairing all select
(use-package elec-pair
  :config
  (dolist (pair '((?\( . ?\))		;; ()
				  (?\[ . ?\])		;; []
				  (?{  . ?})		;; {}
				  (?«  . ?»)		;; «»
				  (?‘  . ?’)		;; ‘’
				  (?‚  . ?‘)		;; ‚‘
				  (?“  . ?”)))	;; “”
	(add-to-list 'electric-pair-pairs pair))
  :hook
  ((adoc-mode
	conf-mode
	emacs-lisp-mode
	markdown-mode
	python-mode
	racket-mode
	scheme-mode
	c++-mode
	clojure-mode 
	ruby-mode) . electric-pair-local-mode))

;; -> Enable global mode autocomplete
(use-package company
  :defer 1
  :config
  (global-company-mode))

;; -> Turn keybindings for move to other window with S-<cursur> keys
(windmove-default-keybindings)

;; -> Turn keybindings for change window size with C-S-<cursor> keys
(use-package windsize
  :config
  (windsize-default-keybindings)	
  (setq windsize-cols 1				;; Set change step to 1 column
		windsize-rows 1))			;; Set change step to 1 row

;; -> HELM
;; https://github.com/emacs-helm/helm
;; Help's and autocomplete input
;; [C-o] — switch between source help's (history or full command list)
(use-package helm
  :ensure t
  :diminish ""
  :config
  (helm-mode 1)
  :bind (:map global-map
			  ("C-x C-f" . helm-find-files)
			  ("C-x b" . helm-buffers-list)
			  ("M-x" . helm-M-x)
			  ("M-y" . helm-show-kill-ring)))

;; -> Help for actual keybindings in EMACS
(use-package which-key
	:ensure t
	:delight ""
	:custom
	(which-key-computer-remaps t "Print actual keybindings, but not IS AS")
	(which-key-idle-delay 2 "Pause befor help")
	(which-key-idle-secondary-delay 0.05 "Second pause befor help")
	(which-key-show-major-mode t "It is a [C-h m], but in format which-key")
	:config
	(which-key-mode 1)
	(which-key-setup-minibuffer))

;; -> EGLOT
;; Package for support LSP.
;; https://elpa.gnu.org/packages/eglot.html
(use-package eglot
  :ensure t
  :defer t
  :config
  ;; Additional servers for various modes
  (add-to-list 'eglot-server-programs '(ansible-mode . ("ansible-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(dockerfile-mode . ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs '(python-mode . ("jedi-language-server")))
  (add-to-list 'eglot-server-programs '(rst-mode . ("esbonio")))
  (add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "rubocop" "--lsp")))
  (add-to-list 'eglot-server-programs '(yaml-mode . ("yaml-language-server"))))

;; -> Enable snippets
(use-package yasnippet
  :defer t
  :config
  (yas-reload-all))

;; -> Add package snippets for yasnippet
(use-package yasnippet-snippets
  :defer t)

;; -> Yasnippet minor-mode enabled for any programming mode and yasnippet mode 
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'emacs-lisp-mode)
			  (eglot-ensure)
              (yas-minor-mode 1))))

(setq inferior-lisp-program "sbcl")	;; Set path to SBCL

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(abyss))
 '(custom-safe-themes
   '("eead109a0c4c72e3926617c5eea8696eb3236ee885a92ee5ab875cec0142c9f2"
	 "93ecd4dc151ca974e989f5d7ada80db450c169ebc31d9f440352f9a66c501212"
	 default))
 '(package-selected-packages
   '(abyss-theme atom-one-dark-theme clojure-snippets flycheck-clojure
				 flycheck-raku helm-cider helm-cider-history
				 helm-clojuredocs helm-company helm-flycheck
				 helm-flyspell helm-lsp helm-org inf-clojure lsp-mssql
				 lsp-scheme lsp-ui magit paredit-everywhere
				 paredit-menu pixel-scroll racket-mode slime
				 vterm-hotkey windsize xterm-color yasnippet-snippets)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
