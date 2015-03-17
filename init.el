;; no splash
(setq inhibit-splash-screen t)

;; no menu
(menu-bar-mode -1)

;; no tabs
(setq-default indent-tabs-mode nil)

;; no backups
(setq make-backup-files nil)

;; show column number
(column-number-mode t)

;; highlight current line
(global-hl-line-mode t)

;; show matching parens
(show-paren-mode t)

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; find files and buffers fast
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; Fortune favors the bold!
(setq initial-scratch-message ";; fortis Fortuna adiuvat")
