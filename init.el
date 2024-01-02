;;; package --- My Emacs config.

;;; Commentary:

;; An attempt at bending Emacs to my will.  Without doing too
;; much bending...

;;; Code:

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
  (global-hl-line-mode t)
  (menu-bar-mode -1)
  (set-face-attribute 'default nil :height 160)
  (setq column-number-mode t)
  (setq custom-file "~/.emacs.c/custom.el")
  (setq echo-keystrokes 0.02)
  (setq frame-resize-pixelwise t)
  (setq inhibit-startup-screen t)
  (setq initial-scratch-message "")
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq require-final-newline t)
  (show-paren-mode t)
  (tool-bar-mode -1))

;;
;; Settings for built-in packages
;;

;; If a file that's open in a buffer changes on disk, update the file
;; in Emacs. (One use case is when doing git pull with files in the
;; project already open; convenient not having to manually re-read
;; file from disk.)
(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode +1))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

;; Let's not create lockfiles and backup files, I'll rely on version
;; control using for example git for files I care about.
(use-package files
  :ensure nil
  :config
  (setq create-lockfiles nil)
  (setq make-backup-files nil))

(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package scroll-bar
  :ensure nil
  :config
  (scroll-bar-mode -1))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(use-package org
  :config
  (setq org-startup-truncated ()))

;; Facilitate running Scheme as an Emacs subprocess.
(use-package xscheme)

;;
;; Third-party packages
;;

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode))

;; Let's see if we can become organized, reflect and learn by keeping
;; a journal.
(use-package org-journal
  :config
  (setq org-journal-dir "/mnt/c/Users/mikae/Dropbox/Documents/journal"))

(provide 'init)
;;; init.el ends here
