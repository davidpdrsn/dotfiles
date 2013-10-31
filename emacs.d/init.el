(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/")
(require 'better-defaults)

;; don't show the GNU splash screen
(setq inhibit-startup-message t)

;; turn off beep warnings
(setq visible-bell 1)

;; highlight matching parens
(show-paren-mode t)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; indent with spaces
(setq-default indent-tabs-mode nil)

;; tab with is 2
(setq default-tab-width 2)

(set-face-attribute 'default nil
                    :family "Ubuntu Mono" :height 180 :weight 'normal)

;; SML
(setq sml-mode-dir "~/.emacs.d/sml-mode/")
(setq sml-program-name "mosml")
(setq sml-default-arg "-P full")
(setq sml-config-file (concat sml-mode-dir "/print.sml"))

(add-to-list 'load-path sml-mode-dir)
(autoload 'sml-mode "sml-mode" () t)
(setq auto-mode-alist (cons '("\\.sml$" . sml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sig$" . sml-mode) auto-mode-alist))
(add-hook 'sml-mode-hook (lambda () (setq sml-indent-level 2)))
