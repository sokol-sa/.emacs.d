(menu-bar-mode -1)
(tool-bar-mode -1)
(tab-bar-mode t)
(setq inhibit-splash-screen t)
(if (display-graphic-p)
    (scroll-bar-mode -1))
(setq-default initial-scratch-message nil)
(setq use-file-dialog nil)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)
(setq auto-save-default nil)

(recentf-mode t)
(ido-mode 1)

(global-set-key (kbd "TAB") 'self-insert-command)
(setq-default tab-width 4)

(global-set-key "\C-x\ \C-g" 'recentf-open-files)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;(unless (package-installed-p 'use-package)			;; Uncomment this if needed oldest version EMACS <30
;;	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(dolist (package-need-install '(atom-one-dark-theme company magit racket-mode slime tramp windsize xterm-color))
  (eval `(use-package ,package-need-install)))

(global-company-mode)

(setq redisplay-dont-pause t
      scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(windmove-default-keybindings)

;;(require 'windsize)
(windsize-default-keybindings)
(setq windsize-cols 1)
(setq windsize-rows 1)

(setq inferior-lisp-program "sbcl")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(atom-one-dark))
 '(custom-safe-themes
   '("93ecd4dc151ca974e989f5d7ada80db450c169ebc31d9f440352f9a66c501212"
	 default))
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
