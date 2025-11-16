;; Start initialization
(custom-set-variables
 '(menu-bar-mode nil "Hide menu on top")
 '(tab-bar-mode t "Enable tab's on top")
 '(use-short-answers t "y and n for answers")
 '(inhibit-splash-screen t "Disable start screen EMACS")
 '(initial-scratch-message nil "Disable message on top SCRATCH buffer")
 '(use-file-dialog nil "Ask for textual confirmation instead of GUI")
 '(make-backup-files nil "Don't make backup file")
 '(auto-save-list-file-name nil "Don't make autosave file list")
 '(auto-save-default nil "Disable default autosave files buffer")
 '(recentf-mode t "Enable mode of recent files")
 '(ido-mode #'both "Enable extra open file mode")
 '(save-place-mode t "Enable restore cursor position in file")
 '(delete-selection-mode t "Enable delete marked text with typing or DEL, BACKSPACE")
 '(ring-bell-function #'ignore "Shut up the bell")
 '(dired-kill-when-opening-new-dired-buffer t "Delete buffer when goto next directory"))

(when (fboundp 'tool-bar-mode)						;; If EMACS run in GUI mode
  (custom-set-variables
   '(tool-bar-mode nil "Disable bar icon on top")
   '(scroll-bar-mode nil "Disable scrollbar")))

;;(menu-bar-mode -1)									;; Hide menu on top
;;(tab-bar-mode t)									;; Enable tab's on top
;;(setq inhibit-splash-screen t)						;; Disable start screen EMACS
;;(if (display-graphic-p)
;;    (scroll-bar-mode -1))							;; Hide scrollbar in GUI mode
;;(tool-bar-mode -1)									;; Hide bar icon on top

;;(setq-default initial-scratch-message nil)			;; Disable message on top SCRATCH buffer
;; (setq use-file-dialog nil)							;; Ask for textual confirmation instead of GUI
;;(setq make-backup-files nil)						;; Don't make backup file
;;(setq auto-save-list-file-name nil)					;; Don't make autosave list
;;(setq auto-save-default nil)						;; Diasable default autosave files buffer

;;(recentf-mode t)									;; Enable menu of recent files

;;(ido-mode 1)										;; Enable extra open file mode

;;(defalias 'yes-or-no-p 'y-or-n-p)					;; Use y and n for answer

;;(setq ring-bell-function 'ignore)					;; shut up the bell

(keymap-global-set "TAB" 'self-insert-command)		;; Enable insert indent with TAB-key
(setq-default tab-width 4)							;; Set TAB indent in 4 symbols

;; Define function for align all comments
(defun align-comments ()
  "Align all comments in region by comment char."
  (interactive)															
  (align-regexp (region-beginning) (region-end)							
                (concat "\\(\\s-*\\)" (regexp-quote comment-start))))	
(keymap-global-set "C-c a c" 'align-comments)		;; Set keybindings for align-comments

(keymap-global-set "C-x C-g" 'recentf-open-files)	;; Set keybindings for open menu recent files

;; Packages initialization
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Set priorities of archives
(custom-set-variables
	'(package-archive-priorities
        '(("gnu" . 40)
          ("nongnu" . 30)
          ("melpa-stable" . 20)
          ("melpa" . 10)))
 	'(package-native-compile t "Compile packages from install but not first start"))

(unless package-archive-contents
  (message "Refresh list archives")
  (package-refresh-contents))						;; Refresh packages list if it not present

;;(unless (package-installed-p 'use-package)			;; Uncomment this if needed oldest version EMACS <30
;;	(package-install 'use-package))

(require 'use-package)								;; Enable to use 'use-package package
(setq use-package-always-ensure t)					;; Always check present package befor install

;; Install needed packages
(dolist (package-need-install '(atom-one-dark-theme
								company magit racket-mode
								slime tramp windsize
								xterm-color
								cider clojure-mode clojure-snippets
								helm helm-cider paredit yasnippet
								yasnippet-snippets))
  (eval `(use-package ,package-need-install)))

;; Help for actual keybindings in EMACS
(use-package which-key
	:ensure t
	:delight ""
	:custom
	(which-key-computer-remaps t "Print actual keybindings, but not IS AS")
	(which-key-idle-delay 2 "Pause befor help")
	(which-key-idle-secondary-delay 0.05 "More pause befor help")
	(which-key-show-major-mode t "It is a [C-h m], but in format which-key")
	:config
	(which-key-mode 1)
	(which-key-setup-minibuffer))

;; Builtin package. Save and restore EMACS state between session
(use-package desktop
	:custom
	(desktop-auto-save-timeout 20 "Autosave every 20 second")
	(desktop-load-locked-desktop t "Load desktop-file but if it blocked")
	(desktop-restore-frames t "Restore frame state")
	(desktop-save t "Save state desktop without questions")
	:config
	;; Modes buffersnot need save and restore
	(add-to-list 'desktop-modes-not-to-save 'dired-mode)
	(add-to-list 'desktop-modes-not-to-save 'Info-mode)
	(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
	;; Switch mode to enable
	(desktop-save-mode 1))

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
