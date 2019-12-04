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
 '(package-selected-packages
   (quote
    (counsel-etags counsel use-package magit-todos flycheck-rtags flycheck-irony lsp-java flycheck pinentry expand-region call-graph google-c-style undo-tree company-irony-c-headers autopair flycheck-rust racer rust-mode company-irony irony company-lsp function-args helm-gtags disable-mouse edit-indirect markdown-mode magit helm helm-projectile smooth-scrolling))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; disable top toolbar
(tool-bar-mode -1)
;; disable top menubar
(menu-bar-mode -1)
;; disable scroll-bar
(toggle-scroll-bar -1)
;; enable time mode in the status bar
(display-time-mode 1)
;; set time hour:min in 24 hour format
(setq display-time-format "%H:%M")
;; don't use tabs
(setq-default indent-tabs-mode nil)
;; set line numbering relative to the cursor
(setq-default display-line-numbers 'relative)
;; set tab width of 4 characters
(setq-default c-basic-offset 4)
;; load misterioso theme
(load-theme 'misterioso)
;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; Highlights matching parenthesis
(show-paren-mode 1)

;; Don't use the weird setup with the control panel in a separate frame.
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
;; Split the windows horizontally instead of vertically
(setq ediff-split-window-function #'split-window-horizontally)
;; When you quit an Ediff session it just leaves the two diff windows around,
;; instead of restoring the window configuration from when Ediff was started.
;; Here's the (slightly hacky) code to restore the old window configuration.
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)
;; Change the horrible ediff colors
(add-hook 'ediff-load-hook
          (lambda ()
            (set-face-foreground
             ediff-current-diff-face-A "#ffffff")
            (set-face-background
             ediff-current-diff-face-A "#6551d4")
            (set-face-foreground
             ediff-fine-diff-face-A "#ffffff")
            (make-face-bold
             ediff-fine-diff-face-A)
            (set-face-background
             ediff-fine-diff-face-A "#4026a9")
            (set-face-background
             ediff-odd-diff-face-A "#221122")
            (set-face-foreground
             ediff-odd-diff-face-A "#ffffff")
            (set-face-foreground
             ediff-current-diff-face-B "#ffffff")
            (set-face-background
             ediff-current-diff-face-B "#6551d4")
            (set-face-foreground
             ediff-fine-diff-face-B "#ffffff")
            (make-face-bold
             ediff-fine-diff-face-B)
            (set-face-background
             ediff-fine-diff-face-B "#4026a9")
            (set-face-background
             ediff-odd-diff-face-B "#221122")
            (set-face-foreground
             ediff-odd-diff-face-B "#ffffff")
            (set-face-background
             ediff-odd-diff-face-A "#221122")
            (set-face-foreground
             ediff-odd-diff-face-A "#ffffff")
            (set-face-background
             ediff-even-diff-face-B "#221122")
            (set-face-foreground
             ediff-even-diff-face-B "#ffffff")
            (set-face-background
             ediff-even-diff-face-A "#221122")
            (set-face-foreground
             ediff-even-diff-face-A "#ffffff")
            ))

;; Set cursor color to green
(set-cursor-color "#9EFF00")
;; Set active modeline color to green
(set-face-attribute 'mode-line nil
                    :background "#9EFF00"
                    :foreground "black"
                    :overline nil
                    :underline nil)
;; Set active modeline color to grey
(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :overline nil
                    :underline nil)

;; if macos
(if (eq system-type 'darwin)
  ;; map the emacs meta (M) key to the command kb key
  (setq mac-command-modifier 'meta))
(put 'upcase-region 'disabled nil)

;; automatically switch point to the newly created splitted window
;; after creation for both vertically and horizontally
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(eval-when-compile
  (require 'use-package))

(use-package whitespace
  :ensure t
  :init
  (setq-default whitespace-style '(face tab-mark lines-tail trailing))
  (setq-default whitespace-line-column 120)
  (setq whitespace-global-modes '(c-mode c++-mode rust-mode emacs-lisp-mode))
  :config
  (global-whitespace-mode t))

;; use smooth scrolling in buffers
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode t))

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode t))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; TODO check if loading after magit is really needed
(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode t))

(use-package cc-mode
  :defer t
  :config
  ;; Provides the google C/C++ coding style.
  (use-package google-c-style
    :ensure t
    :init
    (add-hook 'c-mode-common-hook
      (lambda ()
        (google-set-c-style)
        ;; If you want the RETURN key to go to the next
        ;; line and space over to the right place
        (google-make-newline-indent)))
    :config
    (c-set-offset 'statement-case-open 0)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

(use-package call-graph
  :ensure t
  :bind ("C-c C-g" . call-graph))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package autopair
  :ensure t
  :config
  ;; enable autopair in all buffers
  (autopair-global-mode))

(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

;; todo check counsel dependency to ivy and if one should start before the other
(use-package counsel
  :ensure t
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

(use-package counsel-etags
  :ensure t
  :bind (("M-." . counsel-etags-find-tag-at-point)
         ("M-t" . counsel-etags-grep-symbol-at-point)
         ("M-s" . counsel-etags-find-tag))
  :init
  (add-hook 'prog-mode-hook
    (lambda ()
      (add-hook 'after-save-hook
                'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  ;; Ignore files above 800kb
  ;; (setq counsel-etags-max-file-size 800)
  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  ;; How many seconds to wait before rerunning tags for auto-update
  (setq counsel-etags-update-interval 10)

  ;; ignored files and directories
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories ".vscode")
  (add-to-list 'counsel-etags-ignore-filenames ".clang-format")
  )

;; todo check swiper dependency to ivy and if one should start before the other
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ;; ("M-*" . swiper-under-point)
         ))

(use-package ivy
  :ensure t
  :bind (("C-c C-r" . ivy-resume)
         ("C-s" . swiper-isearch)
         ("M-x" . counsel-M-x))
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package projectile
  :ensure t
  ;; :pin melpa-stable
  ;; :bind* ((:map projectile-mode-map("C-c p" . projectile-command-map))
  ;;         (:map projectile-mode-map("s-p" . projectile-command-map)))
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'ivy)
  ;; (projectile-switch-project-action 'helm-projectile)
  )

;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-mode))

(use-package function-args
  :config
  (fa-config-default)
  (set-default 'semantic-case-fold t)
  ;; Todo check how to set this
  ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))

;; map ibuffer command to C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-c b") 'projectile-compile-project)

;; (global-set-key (kbd "M-f") 'forward-to-word)
;; (global-set-key (kbd "M-b") 'backward-to-word)

;; (require 'helm-config)
;; (require 'helm-gtags)
;; ;; enable helm
;; (helm-mode 1)
;; (setq
;;  helm-gtags-ignore-case t
;;  helm-gtags-auto-update t
;;  helm-gtags-use-input-at-cursor t
;;  helm-gtags-pulse-at-cursor t
;;  helm-gtags-prefix-key "\C-cg"
;;  helm-gtags-suggested-key-mapping t
;;  )

;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

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

;; Stop on the first error.
(setq compilation-scroll-output 'first-error)
;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)
;; jump to first compilation error
(setq compilation-auto-jump-to-first-error t)

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


(require 'lsp-java)
(add-hook 'java-wmode-hook #'lsp)
