;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             '("gnu" . "https://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package ag
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-mode 1)
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(use-package smex
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'smex)
  ;; the old M-x
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package coffee-mode
  :ensure t
  :config
  (custom-set-variables '(coffee-tab-width 2)))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-ruby-rubocop-executable "~/.rbenv/shims/rubocop")
  (if (fboundp 'global-flycheck-mode)
      (global-flycheck-mode +1)
    (add-hook 'prog-mode-hook 'flycheck-mode)))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode)))

(use-package sass-mode
  :ensure t
  :mode "\\.sass\\'")

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-script-padding 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; visible line numbers are nice when pair and mob programming
;(global-linum-mode t)

;; run Lisp under Emacs
(setq inferior-lisp-program "/usr/bin/clisp")

;; custom file
(setq custom-file "~/.emacs.s/custom.el")

;; customize minibuffer startup message
(defvar current-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))
(defun display-startup-echo-area-message ()
  "Customize minibuffer startup message."
  (message "Hack away, Master %s!" current-user))

;; disable startup screen
(setq inhibit-startup-screen t)

;; disable tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; disable scroll bar
(scroll-bar-mode -1)

;; disable blinking cursor
(blink-cursor-mode -1)

;; disable bell ring
(setq ring-bell-function 'ignore)

;; let Emacs zone out after a while
(setq zone-timer (run-with-idle-timer 60 t 'zone))

;; highlight the current line
(global-hl-line-mode +1)

;; nice scrolling
(setq scroll-margin 5
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; map meta to command key
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; bind commenting
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; bind cleaning
(global-set-key (kbd "C-c n") 'simple-clean-region-or-buffer)

;; do whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; no tabs
(setq-default indent-tabs-mode nil)

;; no backups
(setq make-backup-files nil)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; set font size (value is in 1/10pt, so 100 will give you 10pt)
(set-face-attribute 'default nil :height 160)

;; show matching parens
(show-paren-mode t)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; stop Ruby mode from auto-inserting encoding comment
(setq ruby-insert-encoding-magic-comment nil)

;; utf-8 always and forever
(prefer-coding-system 'utf-8)

;; set JavaScript indentation offset
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

;; set CSS indentation offset
(setq css-indent-offset 2)

;; ensure that file ends with newline
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean region or buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simple-clean-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun simple-clean-region-or-buffer ()
  "Cleans a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (whitespace-cleanup)
          (message "Cleaned selected region"))
      (progn
        (simple-clean-buffer)
        (whitespace-cleanup)
        (message "Cleaned buffer")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically save buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice switch-to-buffer (before save-buffer-now activate)
  "Save buffer when switching to other buffer."
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  "Save buffer when switching to other (Emacs) window."
  (when buffer-file-name (save-buffer)))

;; save on loss of (Emacs) focus
;; No bueno until I've figured out how to disable Emacs from attempting
;; to auto-save for example the scratch and minibuffer.
;(add-hook 'focus-out-hook 'save-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Advice ido to reopen file as root if current user lacks write
;; permission for it
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allows Emacs to find project based installs of e.g. eslint. See
;; https://github.com/codesuki/add-node-modules-path and it's license.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
