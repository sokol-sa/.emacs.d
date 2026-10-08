;;; early-init.el --- Early startup configuration -*- lexical-binding: t; -*-

;; Цей файл завантажується ДО ініціалізації пакетів і GUI.
;; Офіційна документація: package-enable-at-startup та package-quickstart
;; мають бути встановлені саме тут, а не в init.el.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup optimization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Великий поріг GC на час старту; після завантаження керування бере gcmh.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
;; gcmh керує лише gc-cons-threshold, тому відсоток повертаємо самі.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-percentage 0.1)))

;; Тимчасово вимикаємо file-name-handler-alist, відновлюємо після старту.
(defvar startup-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       startup-file-name-handler-alist))))
          -100)  ; до desktop-read

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs сам активує пакети між early-init.el та init.el.
;; package-quickstart прискорює цю активацію. Після ручних змін
;; (наприклад, package-load-list) виконайте M-x package-quickstart-refresh.
;; package-enable-at-startup за замовчуванням t; вказано явно для наочності.
(setq package-enable-at-startup t
      package-quickstart t)

;; Native compilation: писати попередження в *Warnings*, але не відкривати буфер.
(setq native-comp-async-report-warnings-errors 'silent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI: прибираємо елементи до створення першого фрейму (без "блимання")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
