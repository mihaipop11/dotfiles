;; disable top toolbar
(tool-bar-mode -1)
;; disable top menubar
(menu-bar-mode -1)
;; disable scroll-bar
(toggle-scroll-bar -1)

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (use-package magit-todos flycheck-rtags flycheck-irony lsp-java flycheck pinentry expand-region call-graph google-c-style undo-tree company-irony-c-headers autopair flycheck-rust racer rust-mode company-irony irony company-lsp function-args helm-gtags disable-mouse edit-indirect markdown-mode magit helm helm-projectile smooth-scrolling))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-when-compile
  (require 'use-package))

(use-package whitespace
  :init
  (setq-default whitespace-style '(face tab-mark lines-tail trailing))
  (setq-default whitespace-line-column 120)
  (setq whitespace-global-modes '(c-mode c++-mode rust-mode emacs-lisp-mode))
  :config
  (global-whitespace-mode t))

;; use smooth scrolling in buffers
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode t))

(use-package disable-mouse
  :config
  (global-disable-mouse-mode t))

(use-package magit
  :bind ("C-x g" . magit-status))

;; TODO check if loading after magit is really needed
(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode t))

;; Provides the google C/C++ coding style.
(use-package google-c-style
  :hook ((c-mode-common)
         ;; If you want the RETURN key to go to the next
         ;; line and space over to the right place
         (c-mode-common . google-make-newline-indent)
         ))

;; enable time mode in the status bar
(display-time-mode 1)
;; set time hour:min in 24 hour format
(setq display-time-format "%H:%M")

;; map ibuffer command to C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c b") 'projectile-compile-project)

;; (global-set-key (kbd "M-f") 'forward-to-word)
;; (global-set-key (kbd "M-b") 'backward-to-word)

;; don't use tabs
(setq-default indent-tabs-mode nil)

;; set line numbering relative to the cursor
(setq-default display-line-numbers 'relative)

(require 'projectile)
(helm-projectile-on)
(projectile-global-mode)
;; redefine projectile key bindings
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completpion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)

;; set tab width of 4 characters
(setq-default c-basic-offset 4)

(require 'helm-config)
(require 'helm-gtags)
;; enable helm
(helm-mode 1)
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(require 'function-args)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

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

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin")))

(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("~/.cargo/bin")))

;; C/C++ Autocompletion
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-irony)
(require 'company-irony-c-headers)
(add-hook 'c++-mode-hook #'(lambda ()
  (add-to-list 'company-backends 'company-irony)
  (add-to-list 'company-backends 'company-irony-c-headers)
  ))

(require 'rust-mode)
(require 'racer)
;; Rustup binaries PATH
(setq racer-cmd "~/.cargo/bin/racer")
;; Rust source code PATH
(setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")

(require 'flycheck)
(global-flycheck-mode t)
(require 'flycheck-rust)
(require 'flycheck-irony)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

(require 'flycheck-rtags)

(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; Highlights matching parenthesis
(show-paren-mode 1)
;; Stop on the first error.
(setq compilation-scroll-output 'first-error)
;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)
;; jump to first compilation error
(setq compilation-auto-jump-to-first-error t)

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

; Set cursor color to green
(set-cursor-color "#9EFF00")

(set-face-attribute 'mode-line nil
                    :background "#9EFF00"
                    :foreground "black"
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :overline nil
                    :underline nil)

(defun kill-compilation-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
         (run-with-timer 1 nil
          (lambda (buf)
            (bury-buffer buf)
            (delete-window))
              buffer)))

;; (add-hook 'compilation-finish-functions 'kill-compilation-buffer-if-successful)

;; Switch between cpp and header in C/C++ major mode
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))

(require 'undo-tree)
(global-undo-tree-mode)

(require 'call-graph)
(call-graph)
(global-set-key (kbd "C-c C-g") 'call-graph)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

(require 'lsp-java)
(add-hook 'java-wmode-hook #'lsp)

(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

