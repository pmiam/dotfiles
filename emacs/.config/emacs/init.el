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
                                        ; server
(use-package windmove
  ;; replace this with the tm/functions and new vm
  :bind (:map global-map
              (("C-M-s-f" . windmove-swap-states-right)
               ("C-M-s-p" . windmove-swap-states-up)
               ("C-M-s-b" . windmove-swap-states-left)
               ("C-M-s-n" . windmove-swap-states-down)
               ("C-s-f" . windmove-right)
               ("C-s-p" . windmove-up)
               ("C-s-b" . windmove-left)
               ("C-s-n" . windmove-down))))
					; completion and correction framework
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1)
  (setq vertico-scroll-margin 0
        vertico-count 20
        vertico-resize nil)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
	      ;; minibuffer local
              (("C-n" . vertico-next)
               ("C-p" . vertico-previous)
               ("M-w" . vertico-save)
               ("M-<" . vertico-first)
               ("M->" . vertico-last)
               ("M-{" . vertico-previous-group)
               ("M-}" . vertico-next-group)
               ("TAB" . vertico-insert)
               ("RET" . vertico-exit)
               ("M-RET" . vertico-exit-input))))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; For tramp:
        completion-category-overrides '((file (styles basic partial-completion))))
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              (("M-a" . marginalia-cycle))))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode 1)
  :config
  (setq corfu-auto t
        corfu-auto-prefix 2
        corfu-quit-at-boundary t
        corfu-auto-delay 0
        corfu-cycle t
        corfu-preselect-first nil)
  (unbind-key "RET" corfu-map)
  :hook (shell-mode . (lambda ()
                        (setq-local corfu-quit-at-boundary t
                                    corfu-quit-no-match t
                                    corfu-auto nil))))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  :config
  (setq cape-dabbrev-check-other-buffers nil))
