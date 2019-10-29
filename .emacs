(tool-bar-mode -1)
(menu-bar-mode -1)

(require 'package)
;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (disable-mouse edit-indirect markdown-mode magit helm smooth-scrolling powerline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq default-tab-width 4)
(setq-default display-line-numbers 'relative)

(require 'smooth-scrolling)
(smooth-scrolling-mode)

(require 'magit)

(require 'disable-mouse)
(global-disable-mouse-mode)

(require 'helm-config)
(helm-mode 1)

(require 'powerline)
(powerline-default-theme)

(load-theme 'misterioso)

(if (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))
