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
					; failure recovery
(use-package recentf
  :init
  (recentf-mode 1))

(use-package emacs
  :config
  (if (not (daemonp))
      (desktop-save-mode 1)
    (defun restore-desktop (frame)
      "Restores desktop and cancels hook after first frame opens.
     So the daemon can run at startup and it'll still work"
      (with-selected-frame frame
	(desktop-save-mode 1)
	(let ((desktop-load-locked-desktop t))
          (desktop-read)
          (remove-hook 'after-make-frame-functions 'restore-desktop))))
    (add-hook 'after-make-frame-functions 'restore-desktop))

  (winner-mode 1)

  (setq backup-directory-alist
	'(("." . "~/.local/share/emacs/saves/")))
  (setq backup-by-copying t
	;; symlinked files + metadata
        version-control t
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2)
  (setq auto-save-default t
        auto-save-timeout 60
        auto-save-interval 200))
