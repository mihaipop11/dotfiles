;; disable top toolbar
(tool-bar-mode -1)
;; disable top menubar
(menu-bar-mode -1)

(require 'package)
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
    (disable-mouse edit-indirect markdown-mode magit helm helm-projectile smooth-scrolling powerline))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; enable time mode in the status bar
(display-time-mode 1)
;; set time hour:min in 24 hour format
(setq display-time-format "%H:%M")

;; map ibuffer command to C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; set tab width of 4 characters
(setq default-tab-width 4)

;; set line numbering relative to the cursor
(setq-default display-line-numbers 'relative)

(require 'smooth-scrolling)
;; enable smooth scrolling mode
(smooth-scrolling-mode)

(require 'projectile)
(helm-projectile-on)
(projectile-global-mode)
;; redefine projectile key bindings
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completpion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'disable-mouse)
;; disable mouse
(global-disable-mouse-mode)

(require 'helm-config)
;; enable helm
(helm-mode 1)

(require 'powerline)
;; enable powerline
(powerline-default-theme)

;; load misterioso theme
(load-theme 'misterioso)

;; if macos
(if (eq system-type 'darwin)
  ;; map the emacs meta (M) key to the command kb key
  (setq mac-command-modifier 'meta))
(put 'upcase-region 'disabled nil)

;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

