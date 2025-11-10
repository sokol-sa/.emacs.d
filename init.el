(menu-bar-mode -1)									;; Hide menu on top
(tool-bar-mode -1)									;; Hide bar icon on top
(tab-bar-mode t)									;; Enable tab's on top
(setq inhibit-splash-screen t)						;; Disable start screen EMACS
(if (display-graphic-p)
    (scroll-bar-mode -1))							;; Hide scrollbar in GUI mode
(setq-default initial-scratch-message nil)			;; Disable message on top SCRATCH buffer
(setq use-file-dialog nil)							;; Ask for textual confirmation instead of GUI
(setq make-backup-files nil)						;; Don't make backup file
(setq auto-save-list-file-name nil)					;; Don't make autosave list
(setq auto-save-default nil)						;; Diasable default autosave files buffer

(recentf-mode t)									;; Enable menu of recent files
(ido-mode 1)										;; Enable extra open file mode

(global-set-key (kbd "TAB") 'self-insert-command)	;; Enable insert indent with TAB-key
(setq-default tab-width 4)							;; Set TAB indent in 4 symbols
(setq ring-bell-function 'ignore)					;; shut up the bell
(defalias 'yes-or-no-p 'y-or-n-p)					;; Use y and n for answer

;; Define function for align all comments
(defun align-comments ()
  "Align all comments in region by comment char."
  (interactive)															
  (align-regexp (region-beginning) (region-end)							
                (concat "\\(\\s-*\\)" (regexp-quote comment-start))))	
(global-set-key (kbd "C-c a c") 'align-comments)	;; Set keybindings for align-comments

(global-set-key "\C-x\ \C-g" 'recentf-open-files)	;; Set keybindings for open menu recent files

;; Packages initialization
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

;; Set priorities of archives
(setopt package-archive-priorities
        '(("gnu" . 40)
          ("nongnu" . 30)
          ("melpa-stable" . 20)
          ("melpa" . 10)))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))						;; Refresh packages list if it not present

;;(unless (package-installed-p 'use-package)			;; Uncomment this if needed oldest version EMACS <30
;;	(package-install 'use-package))

(require 'use-package)								;; Enable to use 'use-package package
(setq use-package-always-ensure t)					;; Always check present package befor install

;; Install needed packages
(dolist (package-need-install '(atom-one-dark-theme company magit racket-mode slime tramp windsize xterm-color))
  (eval `(use-package ,package-need-install)))

(global-company-mode)								;; Enable global mode autocomplete

;; Enable smooth scroll buffer
(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(windmove-default-keybindings)						;; Turn keybindings for move to other window with S-<cursur> keys

;;(require 'windsize)
(windsize-default-keybindings)						;; Turn keybindings for change window size with C-S-<cursor> keys
(setq windsize-cols 1)								;; Set change step to 1 column
(setq windsize-rows 1)								;; Set change step to 1 row

(setq inferior-lisp-program "sbcl")					;; Set path to SBCL

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
					(?‘  . ’?)		;; ‘’
					(?‚  . ‘?)		;; ‚‘
					(?“  . ”?)))	;; “”))
	(add-to-list 'electric-pair-pairs pair))
	:hook
	((adoc-mode
		conf-mode
		emacs-lisp-mode
		markdown-mode
		python-mode
		racket-mode
		scheme-mode
		ruby-mode) . electric-pair-local-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(manoj-dark))
 '(custom-safe-themes
   '("93ecd4dc151ca974e989f5d7ada80db450c169ebc31d9f440352f9a66c501212"
	 default))
 '(package-selected-packages
   '(atom-one-dark-theme company magit racket-mode slime vterm
						 vterm-hotkey windsize xterm-color)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
