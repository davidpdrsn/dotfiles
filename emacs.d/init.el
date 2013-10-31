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

(add-to-list 'load-path "~/.emacs.d/vendor/color-theme-6.6.0")
(require 'color-theme)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
(require 'color-theme-solarized)
(color-theme-solarized-dark)

(set-face-attribute 'default nil
                    :family "Ubuntu Mono" :height 180 :weight 'normal)
