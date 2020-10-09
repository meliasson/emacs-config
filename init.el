;;; package --- My Emacs config.

;;; Commentary:

;; An attempt at bending Emacs to my will.  Without doing too
;; much bending...

;;; Code:

;;
;; Advices
;;

(advice-add 'ido-find-file :after #'switch-to-root-if-required)

;;
;; Functions
;;

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
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;
;; Package management
;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Packages
;;

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package super-save
  :ensure t
  :config
  (super-save-mode t))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy)
  (global-set-key "\C-c\C-f" 'projectile--find-file))

(use-package js
  :ensure t
  :config
  (setq js-indent-level 2))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package add-node-modules-path
  :ensure t
  :hook js-mode)

(use-package prettier-js
  :ensure t
  :hook (js-mode . prettier-js-mode))

(use-package magit
  :ensure t)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode))

(use-package json-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (before-save . gofmt-before-save))

(use-package restclient
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

;; (ensure-package 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;;
;; Misc. settings
;;

;; Skip audible ding.
(setq ring-bell-function 'ignore)

;; Set custom file.
(setq custom-file "~/.emacs.s/custom.el")

;; Map meta to command key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Disable tool bar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable scroll bar.
(scroll-bar-mode -1)

;; Increase font size.
(set-face-attribute 'default nil :height 160)

;; Display column number in mode line.
(column-number-mode nil)

;; Bind commenting.
(global-set-key (kbd "M-c") 'comment-or-uncomment-region)

;; Bind cleaning.
(global-set-key (kbd "M-n") 'simple-clean-region-or-buffer)

;; Do whitespace cleanup on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Skip tabs.
(setq-default indent-tabs-mode nil)

;; Skip backups.
(setq make-backup-files nil)

;; Revert buffers automatically when underlying files are changed
;; externally.
(global-auto-revert-mode t)

;; Show matching parentheses.
(show-paren-mode t)

;; y or n instead of yes and no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 always and forever.
(prefer-coding-system 'utf-8)

;; Ensure that files end with newline.
(setq require-final-newline t)

(provide 'init)
;;; init.el ends here
