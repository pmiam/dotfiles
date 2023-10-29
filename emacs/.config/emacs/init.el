;; -*- lexical-binding: t; -*-
					; supplementary files
(setq custom-file (concat user-emacs-directory "custom.el"))
(unless (load custom-file)
  (write-region "" nil custom-file))

(setq alias-file (concat user-emacs-directory "alias.el"))
(unless (load alias-file)
  (write-region "" nil alias-file))
