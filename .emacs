(require 'package)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Install quelpa itself:
(use-package quelpa
  :ensure t
  )

(use-package quelpa-use-package
  :ensure t
  :init (setq quelpa-update-melpa-p nil)
  :config (quelpa-use-package-activate-advice))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
 '(package-selected-packages
   (quote
    (company-jedi counsel-projectile cmake-ide racer company-racer flycheck-rust flycheck-irony company-irony-c-headers company-c-headers company-irony google-c-style rust-mode flycheck modern-cpp-font-lock function-args irony company projectile counsel-gtags counsel pinentry autopair expand-region undo-tree magit-todos magit disable-mouse smooth-scrolling use-package))))
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

;; load cbrew-theme
(use-package cbrew-theme
  :ensure t
  :quelpa (cbrew-theme
           :fetcher github
           :repo "mihaipop11/cbrew-theme"
           )
  :config (load-theme 'cbrew t)
  )

;; disable backup
(setq backup-inhibited t)
;; disable auto save
(setq auto-save-default nil)
;; Highlights matching parenthesis
(show-paren-mode 1)

;; Split window preferred function: vertically
(setq split-height-threshold nil)
(setq split-width-threshold 220)

;; Don't use the weird setup with the control panel in a separate frame.
(setq ediff-window-setup-function #'ediff-setup-windows-plain)
;; Split the windows horizontally instead of vertically
(setq ediff-split-window-function #'split-window-horizontally)
;; When you quit an Ediff session it just leaves the two diff windows around,
;; instead of restoring the window configuration from when Ediff was started.
;; Here's the (slightly hacky) code to restore the old window configuration.
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; if macos
(if (eq system-type 'darwin)
    ;; map the emacs meta (M) key to the command kb key
    (setq mac-command-modifier 'meta))
(put 'upcase-region 'disabled nil)

;; automatically switch point to the newly created splitted window
;; after creation for both vertically and horizontally
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

;; Switch between cpp and header in C/C++ major mode
(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map   (kbd "C-c o") 'ff-find-other-file)
     (define-key c++-mode-map (kbd "C-c o") 'ff-find-other-file)))

;; map ibuffer command to C-x C-b
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Stop on the first error.
(setq compilation-scroll-output 'first-error)

;; Don't stop on info or warnings.
(setq compilation-skip-threshold 2)
;; jump to first compilation error
(setq compilation-auto-jump-to-first-error t)

(global-set-key (kbd "C-c b") 'projectile-compile-project)

;; Overlay windows (What does it do?)
(add-to-list 'display-buffer-alist
             '("\\*Help\\*" display-buffer-in-side-window))
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom) (size . 0.2)))
(add-to-list 'display-buffer-alist
             '("\\*undo-tree\\*" (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right) (size . 0.2)))

;; (defun notify-compilation-result(buffer msg)
;;   "Notify that the compilation is finished,
;; close the *compilation* buffer if the compilation is successful,
;; and set the focus back to Emacs frame"
;;   (if (and
;;        (string-match "^finished" msg)
;;        (not
;;         (with-current-buffer buffer
;;           (goto-char (point-min))
;;           (search-forward "warning" nil t))))
;;       (progn
;;         (delete-windows-on buffer))
;;     )
;;   (setq current-frame (car (car (cdr (current-frame-configuration)))))
;;   (select-frame-set-input-focus current-frame)
;;   )
(defun notify-compilation-result(buffer msg)
  "Notify that the compilation is finished,
close the *compilation* buffer if the compilation is successful,
and set the focus back to Emacs frame"
  (if (string-match "^finished" msg)
      (progn
        (delete-windows-on buffer))
    )
  (setq current-frame (car (car (cdr (current-frame-configuration)))))
  (select-frame-set-input-focus current-frame)
  )
(add-hook 'compilation-finish-functions 'notify-compilation-result)


(eval-when-compile
  (require 'use-package)
  )

(use-package whitespace
  :ensure t
  :init
  (setq-default whitespace-style '(face tab-mark lines-tail trailing))
  (setq-default whitespace-line-column 120)
  (setq whitespace-global-modes '(c-mode c++-mode rust-mode emacs-lisp-mode python-mode))
  :config
  (global-whitespace-mode t)
  )

;; use smooth scrolling in buffers
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode t)
  )

(use-package disable-mouse
  :ensure t
  :config
  (global-disable-mouse-mode t)
  )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  )

;; TODO check if loading after magit is really needed
(use-package magit-todos
  :disabled
  :ensure t
  :after magit
  :config
  (magit-todos-mode t)
  )

(use-package cc-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
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
    (c-set-offset 'statement-case-open 0)
    )
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  )

(use-package call-graph
  :disabled
  :ensure t
  :bind ("C-c C-g" . call-graph)
  )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region)
  )

(use-package autopair
  :ensure t
  :config
  ;; enable autopair in all buffers
  (autopair-global-mode)
  )

(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  :config
  (pinentry-start)
  )

;; todo check counsel dependency to ivy and if one should start before the other
(use-package counsel
  :ensure t
  ;;:pin melpa-stable
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c C-m" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git")
  )

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  )

(use-package counsel-etags
  :disabled ;; Using counsel-gtags instead
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
  (setq counsel-etags-update-interval 60)

  ;; ignored files and directories
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories ".vscode")
  (add-to-list 'counsel-etags-ignore-filenames ".clang-format")
  )

(use-package ggtags
  :disabled
  :ensure t
  :commands ggtags-mode
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (ggtags-mode t))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )

(use-package counsel-gtags
  ;; :disabled
  :ensure t
  :pin melpa-stable
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (counsel-gtags-mode t))))
  :config
  (setq counsel-gtags-auto-update t
        counsel-gtags-path-style 'root
        )
  :bind (:map
         counsel-gtags-mode-map
         ("M-t" . counsel-gtags-find-definition)
         ("M-r" . counsel-gtags-find-reference)
         ("M-s" . counsel-gtags-find-symbol)
         ("M-." . counsel-gtags-dwim)
         ("M-," . counsel-gtags-go-backward))
  )

;; todo check swiper dependency to ivy and if one should start before the other
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("M-*" . swiper-under-point))
  )

(use-package ivy
  :ensure t
  :bind ("C-c C-r" . ivy-resume)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  )

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

(use-package irony
  :ensure t
  :commands irony-mode
  ;; standard irony configuration
  :bind (:map irony-mode-map
              ("C-c C-b" . irony-cdb-menu)
              ("C-c =" . irony-get-type))
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  :config
  (use-package company-irony
    :ensure t
    :config
    (setq company-irony-ignore-case 'smart)
    (add-to-list 'company-backends 'company-irony)

    (use-package company-irony-c-headers
      :disabled
      :ensure t
      :init
      (add-to-list 'company-backends 'company-irony-c-headers)
      )
    )

  (use-package flycheck-irony
    :ensure t
    :commands flycheck-irony-setup
    :init
    (add-hook 'c++-mode-hook 'flycheck-irony-setup)
    (add-hook 'c-mode-hook 'flycheck-irony-setup)
    (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
    )

  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  ;; (defun my-irony-mode-hook ()
  ;;   (define-key irony-mode-map [remap completion-at-point]
  ;;     'irony-completion-at-point-async)
  ;;   (define-key irony-mode-map [remap complete-symbol]
  ;;     'irony-completion-at-point-async))
  ;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  )

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode)
  :bind (("<backtab>" . company-complete-common-or-cycle))
  :config
  ;;  (setq company-backends (delete 'company-clang company-backends))
  ;;  (delete 'company-backends 'company-clang)

  (use-package company-c-headers
    :ensure t
    :functions company-c-headers
    :config
    (add-to-list 'company-backends 'company-c-headers)
    )

  ;;(setq company-backends '(company-c-headers
  ;;                         (company-gtags)))
  )

(use-package cmake-ide
  :ensure t
  :init
  (use-package semantic/bovine/gcc)
  (setq cmake-ide-flags-c++ (append '("-std=c++20")
                                    (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
  (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
  (cmake-ide-setup)
  )

(use-package function-args
  :ensure t
  :config
  (fa-config-default)
  (set-default 'semantic-case-fold t)
  ;; Todo check how to set this
  ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(use-package modern-cpp-font-lock
  :ensure t
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-mode . modern-c++-font-lock-mode)
  )

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  )

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

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (global-company-mode)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq exec-path (append exec-path '("~/.cargo/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin")))
  :config
  (defun my-rust-mode-hook()
    (set (make-local-variable 'compile-command "cargo run"))
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
    ;;(set (make-local-variable 'company-backends) '(company-racer))
    ;;(local-set-key (kbd "TAB") #'racer-complete-or-indent)
    )
  (add-hook 'rust-mode-hook 'my-rust-mode-hook)
  (add-hook 'rust-mode-hook #'racer-mode)

  (use-package flycheck-rust
    :ensure t
    :defer t
    )

  (use-package company-racer
    :ensure t
    :defer t
    )

  (use-package racer
    :ensure t
    :defer t
    :init
    (setq racer-rust-src-path (concat (getenv "HOME")
                                      "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src"))
    (setq racer-cmd (concat (getenv "HOME")
                            "~/.cargo/bin/racer"))
    :config
    (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t)
    )
  )

;; company-jedi wires up jedi to be a backend for the auto completion
;; library, company-mode.
(use-package company-jedi
  :ensure t
  :config
  :hook
  ((python-mode . jedi:setup))
  :init
  (setq jedi:complete-on-dot t)
  (setq jedi:use-shortcuts t)
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'company-backends 'company-jedi)))
  )

;; (require 'flycheck-rtags)
;; (defun my-flycheck-rtags-setup ()
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;   (setq-local flycheck-check-syntax-automatically nil))
;; ;; c-mode-common-hook is also called by c++-mode
;; (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; (require 'lsp-java)
;; (add-hook 'java-wmode-hook #'lsp)
