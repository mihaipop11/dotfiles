(require 'package)

(package-initialize)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; inhibit startup message
(setq inhibit-startup-message t)
;; disable top toolbar
(tool-bar-mode -1)
;; disable top menubar
(menu-bar-mode -1)
;; disable scroll-bar
(scroll-bar-mode -1)
;; Set up the visible bell
(setq visible-bell t)
;; enable time mode in the status bar
(display-time-mode 1)
;; set time hour:min in 24 hour format
(setq display-time-format "%H:%M")
;; show column numbers in statusbar
(setq column-number-mode t)
;; don't use tabs
(setq-default indent-tabs-mode nil)
;; set line numbering relative to the cursor
(setq-default display-line-numbers 'absolute)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-nord-aurora))

;; (load-theme 'zmpi)

(setq org-image-actual-width nil)
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

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

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
;; map f5 key to kill the compilation window
(defun kill-compilation-window ()
  "Run compile and resize the compile window closing the old one if necessary"
  (interactive)
  (progn
    (delete-windows-on (get-buffer "*compilation*")) ; Delete the compilation windows
    ;;(kill-buffer "*compilation*") ; and kill the buffers
    ))
(global-set-key [f5] 'kill-compilation-window)

(defvar parameters
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

(setq fit-window-to-buffer-horizontally t)
(setq window-resize-pixelwise t)

(setq electric-pair-mode t)

(setq
 display-buffer-alist
 `(("\\*undo-tree\\*" display-buffer-in-side-window
    (side . right)
    (slot . 0)
    (window-width . fit-window-to-buffer)
    (preserve-size . (t . nil))
    ,parameters)

   ("\\*\\(?:shell\\|compilation\\)\\*" display-buffer-in-side-window
    (side . bottom)
    (slot . 0)
    (window-height . 18)
    (preserve-size . (nil . t))
    ,parameters)
   ))

(eval-when-compile
  (require 'use-package))

(use-package whitespace
  :ensure t
  :init
  (setq-default whitespace-style '(face tab-mark lines-tail trailing))
  (setq-default whitespace-line-column 120)
  ;; TODO next line should not be defined in this context and should match with
  ;; the whitespace-line-column
  (setq-default fill-column 120)
  (setq whitespace-global-modes '
        (c-mode
         c++-mode
         rust-mode
         emacs-lisp-mode
         python-mode
         org-mode
         java-mode))
  :config
  (global-whitespace-mode t))

;; use smooth scrolling in buffers
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode t))

(use-package disable-mouse
  :ensure t
  :diminish
  :config
  (global-disable-mouse-mode t))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :ensure t
  :after magit
  :config
  (magit-todos-mode t))

(use-package forge
  :ensure t
  :diminish
  :after magit)

(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode t))

(with-eval-after-load 'undo-tree
  (setq undo-tree-auto-save-history nil))

(use-package call-graph
  :disabled
  :ensure t
  :bind ("C-c C-g" . call-graph))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package pinentry
  :ensure t
  :init
  (setq epa-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package counsel
  :ensure t
  :bind*
  (("M-x" . counsel-M-x)
   ("C-c b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file))
  :custom
  (counsel-find-file-ignore-regexp "\\.DS_Store\\|.git"))

;; todo check swiper dependency to ivy and if one should start before the other
(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
         ("M-*" . swiper-under-point)))

(use-package ivy
  :diminish
  :ensure t
  :bind (("C-s" . swiper)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :ensure t
  :init (ivy-rich-mode 1))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; (global-set-key (kbd "C-c b") 'projectile-compile-project)
(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode)
  (setq projectile-track-known-projects-automatically nil)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  :config
  (setq company-tooltip-align-annotations t))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))


;; set propper major mode for newly created
;; buffers without a file associated with it
(setq-default major-mode
              (lambda () (if buffer-file-name
                             (fundamental-mode)
                           (let ((buffer-file-name (buffer-name)))
                             (set-auto-mode)))))

;; (use-package cmake-mode
;;   :ensure t
;;   :mode "CMakeLists.txt")

;; (use-package cmake-ide
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\*cmake\\*" . compilation-mode))
;;   :init
;;   (use-package semantic/bovine/gcc)
;;   (setq cmake-ide-flags-c++ (append '("-std=c++20")
;;                                     (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
;;   (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
;;   (cmake-ide-setup)
;;   )

(use-package function-args
  :ensure t
  :config
  (fa-config-default)
  (set-default 'semantic-case-fold t)
  ;; Todo check how to set this
  ;; (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  )

(use-package flycheck
  :ensure t
  :init
  ;; (global-flycheck-mode)
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook
          (lambda () (progn
                       (setq flycheck-cppcheck-standards '("c++20"))
                       (setq flycheck-clang-language-standard "c++20")
                       (setq flycheck-gcc-language-standard "c++20"))))
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  )

(use-package flycheck-google-cpplint
  :ensure t
  :after flycheck lsp-mode lsp-ui-flycheck
  :config (progn
          (flycheck-add-next-checker 'c/c++-cppcheck '(t . c/c++-googlelint)))
  )

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin"))))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l") ;; (few alternatives - "C-l", "s-l")
  :hook
  ((c-mode c++-mode rust-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :custom
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-enable-indentation t)
  (setq lsp-clients-clangd-args '("--header-insertion-decorators=0"))
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  ;; (setq lsp-diagnostics-provider :flycheck)
  )

(use-package lsp-ui
  :ensure t
  :requires lsp-mode flycheck
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-enable t)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-peek-enable t)
  ;; (setq lsp-ui-peek-show-directory nil)
  ;; (setq lsp-ui-peek-peek-height 40)
  :hook (lsp-mode . lsp-ui-mode)
  )

;; add cppcheck checker in flycheck after lsp
;; https://github.com/flycheck/flycheck/issues/1762
(defvar-local my/flycheck-local-cache nil)
(defun my/flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker my/flycheck-local-cache))
      (funcall fn checker property)))
(advice-add 'flycheck-checker-get :around 'my/flycheck-checker-get)
(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'c++-mode)
              (setq my/flycheck-local-cache '((lsp . ((next-checkers . (c/c++-cppcheck)))))))))


;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t
  :commands (dap-debug)
  :config (require 'dap-gdb-lldb) ; gdb mode
          (dap-mode 1)
          (dap-tooltip-mode 1)
          (dap-ui-mode 1)
          (dap-auto-configure-mode))
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.2))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-project-dir)
  (setq neo-theme (if (display-graphic-p) 'nerd 'arrow))
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (setq neo-window-width 40))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package diminish
  :ensure t)

(use-package qml-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :custom
  (plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :config
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  ;;(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (setq plantuml-output-type "png")
  (setq plantuml-options "-charset UTF-8"))

(use-package meson-mode
  :ensure t
  :mode "\\meson\\.build\\'"
  :init
  (add-hook 'meson-mode-hook 'company-mode)
  )

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  :init
  (add-hook 'rust-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  )

