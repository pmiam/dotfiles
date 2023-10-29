;; -*- lexical-binding: t; -*-
					; supplementary files
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (load custom-file)
  (write-region "" nil custom-file))

(setq alias-file (concat user-emacs-directory "alias.el"))
(unless (load alias-file)
  (write-region "" nil alias-file))
					; package repositories and use-package macro
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))
					; user experience
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-dark+ t))

(use-package emacs
  :config
  ;; (setq scroll-conservatively 5)
  (column-number-mode 1)
  (setq visible-bell t)
  (setq inhibit-startup-screen t))
					; user interface
(use-package emacs
  :config
  (setq skeleton-pair t)
  :bind (:map global-map
              (("C-S-d" . backward-delete-char-untabify)
               ("C-%" . replace-regexp)
               ("(" . skeleton-pair-insert-maybe)
               ("[" . skeleton-pair-insert-maybe)
               ("{" . skeleton-pair-insert-maybe)
               ("\"" . skeleton-pair-insert-maybe))))

(global-set-key (kbd "C-z") 'ignore)
(global-set-key (kbd "C-x C-z") 'ignore)
