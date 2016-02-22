;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; No splash.
(setq inhibit-splash-screen t)

;; No menu.
(menu-bar-mode -1)

;; No tabs.
(setq-default indent-tabs-mode nil)

;; No backups.
(setq make-backup-files nil)

;; Show column number.
(column-number-mode t)

;; Highlight current line.
(global-hl-line-mode t)

;; Show matching parens.
(show-paren-mode t)

;; Do whitespace cleanup on save.
(add-hook 'before-save-hook 'whitespace-cleanup)

;; y or n.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Bind commenting.
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Bind cleaning.
(global-set-key (kbd "C-c n") 'simple-clean-region-or-buffer)

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
          (message "Cleaned selected region."))
      (progn
        (simple-indent-buffer)
        (whitespace-cleanup)
        (message "Cleaned buffer.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; List packages to install if missing.
(setq package-list '(flycheck smex))

;; List repositories containing the packages.
(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; Load installed packages.
(package-initialize)

;; Fetch list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Avoid loading installed packages again after processing the init file.
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ido
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-x") 'smex)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
