;;; init.el --- Personal Emacs configuration -*- lexical-binding: t; -*-

;; Оптимізація старту, package-quickstart та вимкнення UI-елементів
;; перенесено в early-init.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file (окремо, щоб Customize не засмічував init.el)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(("gnu"    . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa"  . "https://melpa.org/packages/")))
(setq package-archive-priorities
      '(("gnu"    . 40)
        ("nongnu" . 30)
        ("melpa"  . 10)))

;; Оновити список пакетів лише якщо він ще не завантажений (перший запуск).
(unless (file-directory-p (expand-file-name "archives" package-user-dir))
  (package-refresh-contents))

;; use-package є частиною ядра в Emacs 29+
(require 'use-package)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gcmh перебирає керування gc-cons-threshold після великого порогу з early-init.el
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Фрейми вже створені без панелей (early-init.el); тут синхронізуємо стан режимів.
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(menu-bar-mode -1)
(tab-bar-mode 1)
(repeat-mode 1)
(delete-selection-mode 1)

;; cursor-type — per-buffer змінна, тому setq-default.
;; Без перевірки display-graphic-p, щоб працювало і в режимі демона.
(setq-default cursor-type 'bar)

(setq use-short-answers t
      inhibit-splash-screen t
      initial-scratch-message nil
      use-file-dialog nil
      ring-bell-function #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
;; (pixel-scroll-precision-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter: автоматичне встановлення граматик
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File safety
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 5)
(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file "backups"))))
(auto-save-visited-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq dired-kill-when-opening-new-dired-buffer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TAB helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-tab ()
  "Indent region or insert real TAB."
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (unless buffer-read-only
      (insert "\t"))))
(global-set-key (kbd "C-<tab>") #'my-tab)
(setq-default tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Align comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun align-comments (beg end)
  "Align comments inside region."
  (interactive "r")
  (unless comment-start
    (user-error "У цьому режимі синтаксис коментарів не визначено"))
  (align-regexp
   beg end
   (concat "\\(\\s-*\\)" (regexp-quote (string-trim-right comment-start)))))
(global-set-key (kbd "C-c a c") #'align-comments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recent files, Saveplace, Savehist (вбудовані пакети)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Без :defer — щоб файли, відновлені desktop-ом, теж потрапляли в список.
(use-package recentf
  :ensure nil
  :demand t
  :custom
  (recentf-max-saved-items 100)
  (recentf-save-file (locate-user-emacs-file "recentf"))
  :config
  (recentf-mode 1))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-forget-unreadable-files t)
  :config
  (save-place-mode 1))

(use-package savehist
  :ensure nil
  :demand t
  :config
  (savehist-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Desktop (restore session)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; desktop-save-mode сам читає desktop під час старту (шукає в desktop-path,
;; який за замовчуванням містить user-emacs-directory), тому ручний
;; desktop-read і desktop-dirname не потрібні.
(use-package desktop
  :ensure nil
  :custom
  (desktop-auto-save-timeout 20)
  (desktop-load-locked-desktop 'ask)
  (desktop-restore-frames t)
  :config
  (dolist (mode '(dired-mode Info-mode info-lookup-mode))
    (add-to-list 'desktop-modes-not-to-save mode))
  ;; Не відновлювати шрифт/кольори фреймів з desktop-файлу —
  ;; щоб працювали налаштування теми та шрифту з init.el.
  (dolist (param '(font foreground-color background-color
                   background-mode cursor-color))
    (add-to-list 'frameset-filter-alist (cons param :never)))
  (desktop-save-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Аргумент t у load-theme вже пропускає перевірку custom-safe-themes.
(use-package abyss-theme
  :config
  (load-theme 'abyss t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :commands (magit-status magit-dispatch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package slime
  :commands slime
  :config (setq inferior-lisp-program "sbcl"))

(use-package racket-mode
  :mode "\\.rkt\\'")

(use-package cider
  :commands (cider-jack-in cider-connect))

(use-package clojure-mode)

(use-package clojure-snippets
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :commands vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Electric Pair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode 1)
;; Типографські лапки лише для текстових режимів.
;; Українська норма: «основні», „внутрішні“. Пари без конфліктів:
;; жоден символ не є одночасно відкривним в одній парі та закривним в іншій.
(add-hook 'text-mode-hook
          (lambda ()
            (setq-local electric-pair-pairs
                        '((?\" . ?\") (?« . ?») (?„ . ?“) (?‘ . ?’)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paredit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package paredit
  :hook ((emacs-lisp-mode
          lisp-mode
          scheme-mode
          racket-mode
          clojure-mode
          slime-repl-mode
          cider-repl-mode) . paredit-mode)
  :config
  ;; Paredit сам керує дужками: вимикаємо electric-pair, поки paredit активний,
  ;; і повертаємо, якщо paredit вимкнули вручну.
  (add-hook 'paredit-mode-hook
            (lambda () (electric-pair-local-mode (if paredit-mode -1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completion (Vertico stack)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
  :demand t
  :config
  (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("C-s"     . consult-line)
         ("C-x C-g" . consult-recent-file)))  ; з попереднім переглядом

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Embark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package embark
  :bind
  (("C-."   . embark-act)
   ("C-;"   . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-completion: Corfu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package corfu
  :demand t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-quit-no-match t)
  :config
  (global-corfu-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cape (Completion At Point Extensions)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; За README Cape: додаємо capf у ГЛОБАЛЬНИЙ список через add-hook.
;; Буферні capf (Eglot, elisp тощо) мають пріоритет; глобальні спрацюють
;; після них, якщо буферний список закінчується на t.
;; Eglot сам додає свою capf у буферах, де він активний — вручну не треба.
(use-package cape
  :demand t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; add-hook додає на початок, тому порядок спрацювання: file → dabbrev
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; Ключові слова мови — лише в програмних режимах
  (add-hook 'prog-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-keyword 90 t)))
  ;; Elisp-блоки в Markdown/Org
  (add-hook 'text-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions #'cape-elisp-block nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Which-Key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Вбудований з Emacs 30; для старіших версій встановлюється з ELPA.
(use-package which-key
  :defer 2
  :custom
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(windmove-default-keybindings)
(use-package windsize
  :config
  (windsize-default-keybindings)
  (setq windsize-cols 1
        windsize-rows 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config (yas-reload-all))
(use-package yasnippet-snippets
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown (потрібен, щоб спрацьовував хук Eglot для markdown-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eglot (LSP)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml-language-server та marksman уже є в стандартному eglot-server-programs,
;; тому перевизначаємо лише Ruby (ruby-lsp замість дефолтного сервера).
(use-package eglot
  :ensure nil
  :hook ((python-mode    . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (ruby-mode      . eglot-ensure)
         (ruby-ts-mode   . eglot-ensure)
         (yaml-ts-mode   . eglot-ensure)
         (markdown-mode  . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp"))))

;;; init.el ends here
