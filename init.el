;;; package --- My Emacs config.

;;; Commentary:

;; An attempt at bending Emacs to my will.  Without doing too
;; much bending...

;;; Code:

;;
;; Functions
;;

(defun flycheck-rubocop-via-bundle-exec ()
  "Make Flycheck run Rubocop via bundle exec.

Needed when Rubocop is nested inside another gem."
  (make-variable-buffer-local 'flycheck-command-wrapper-function)
  (setq flycheck-command-wrapper-function
        (lambda (command)
          (append '("bundle" "exec") command))))

(defun company-backend-with-yas (backends)
  "Add yasnippet completion to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends) (memq 'company-yasnippet backends))
      backends
    (append (if (consp backends)
                backends
              (list backends))
            '(:with company-yasnippet))))

(defun simple-clean-region-or-buffer ()
  "Cleans region if selected, otherwise the whole buffer.

Indents and removes whitespace."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (whitespace-cleanup)
          (message "Cleaned region"))
      (progn
        (indent-region (point-min) (point-max))
        (whitespace-cleanup)
        (message "Cleaned buffer")))))

(defun switch-to-root-if-required ()
  "Switch to root if required.

If editing of current buffer requires root privileges, this function
reopens file as root."
  (interactive)
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun display-startup-echo-area-message ()
  "Customize minibuffer startup message."
  (message "(ง ͡ʘ ͜ ͡ʘ)ง"))

;;
;; Package management
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t))

;;
;; Non-package settings
;;

(use-package emacs
  :config
  (blink-cursor-mode -1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Show line numbers. (Nice when pair and mob programming.)
  ;; (global-display-line-numbers-mode t)
  (global-hl-line-mode t)
  (global-set-key (kbd "M-c") 'comment-or-uncomment-region)
  (global-set-key (kbd "M-n") 'simple-clean-region-or-buffer)
  (set-face-attribute 'default nil :height 160)
  (setq auto-window-vscroll nil)
  (setq column-number-mode t)
  (setq custom-file "~/.emacs.s/custom.el")
  ;; (setq-default default-directory "/Users/mikaeleliasson/")
  (setq echo-keystrokes 0.02)
  (setq frame-resize-pixelwise t)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  ;; Map meta to command key.
  (setq mac-command-modifier 'meta)
  ;; Type brackets and curly braces with alt keys.
  (setq mac-option-modifier nil)
  (setq ring-bell-function 'ignore)
  (setq scroll-conservatively 101)
  (setq-default indent-tabs-mode nil)
  (setq-default line-spacing 3)
  (setq require-final-newline t)
  (show-paren-mode t)
  (tool-bar-mode -1))

;;
;; Settings for built-in packages
;;

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode +1))

(use-package files
  :ensure nil
  :config
  (setq create-lockfiles nil)
  (setq make-backup-files nil))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1))

(use-package js
  :ensure nil
  :hook (js-mode . (lambda () (set-fill-column 80)))
  :config
  (setq js-indent-level 2))

(use-package css-mode
  :ensure nil
  :config
  (setq css-indent-offset 2))

(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package dired
  :ensure nil
  :hook (dired-mode . dired-hide-details-mode))

;;
;; Third-party packages
;;

(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (ivy-mode 1))

(use-package counsel
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package swiper
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package smex)

(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package projectile
  :config
  (projectile-mode +1)
  ;; (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "C-c c") #'projectile-ag)
  (define-key projectile-mode-map (kbd "C-c f") #'projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package super-save
  :config
  (super-save-mode t))

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :hook js-mode)

(use-package prettier-js
  :hook (js-mode . prettier-js-mode))

(use-package magit)

(use-package nyan-mode
  :config
  (nyan-mode 1))

(use-package json-mode
  :ensure t)

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil))))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  ;; Add yasnippet to all backends.
  (setq company-backends
        (mapcar #'company-backend-with-yas company-backends))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package yaml-mode)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;; (use-package go-mode
;;   :ensure t
;;   :hook (before-save . gofmt-before-save))

;; (use-package rubocop
;;   :ensure t
;;   :hook (ruby-mode . rubocop-mode)
;;   :hook (rubocop-mode . flycheck-rubocop-via-bundle-exec))

(provide 'init)
;;; init.el ends here
