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
;; show column numbers in statusbar
(setq column-number-mode t)
;; don't use tabs
(setq-default indent-tabs-mode nil)
;; set line numbering relative to the cursor
(setq-default display-line-numbers 'relative)
;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; set tab width of 4 characters
(setq-default c-basic-offset 4)

(load-theme 'zmpi)

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

;; Overlay windows (What does it do?)
(add-to-list 'display-buffer-alist
             '("\\*Help\\*" display-buffer-in-side-window))
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom) (size . 0.2)))
(add-to-list 'display-buffer-alist
             '("\\*Org-Babel Error Output\\*" (display-buffer-reuse-window display-buffer-in-side-window)
               (side . bottom) (size . 0.2)))
(add-to-list 'display-buffer-alist
             '("\\*undo-tree\\*" (display-buffer-reuse-window display-buffer-in-side-window)
               (side . right) (size . 0.2)))


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
  :config
  (global-disable-mouse-mode t))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-todos
  :disabled
  :ensure t
  :after magit
  :config
  (magit-todos-mode t))

(use-package cc-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  ;; Provides the google C/C++ coding style.
  (use-package google-c-style
    :disabled
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
  :diminish
  :config
  (global-undo-tree-mode t))

(use-package call-graph
  :disabled
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
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-completion-system 'ivy))

(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package ccls
  :after lsp
  :ensure t
  :config
;;  (setq ccls-executable "ccls")
  ;; (setq ccls-executable "/home/mihai/work/ccls/Release/ccls")
  (with-eval-after-load 'projectile
    (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package company
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

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

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

(use-package modern-cpp-font-lock
  :ensure t
  :disabled
  :diminish modern-c++-font-lock-mode
  :hook
  (c++-mode . modern-c++-font-lock-mode))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package rust-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setenv "PATH" (concat (getenv "PATH") (substitute-in-file-name ":$HOME/.cargo/bin"))))


;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l") ;; (few alternatives - "C-l", "s-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         ;; (lsp-mode . efs/lsp-mode-setup)
         (rust-mode . lsp)
         (c++-mode . lsp)
         (cc-mode . lsp))
  :commands lsp
  :config
  ;; (setq lsp-completion-provider :capf))
  (add-hook 'lsp-managed-mode-hook (lambda () (setq-local company-backends '(company-capf)))))

;; optionally
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-peek-peek-height 40))

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode
  :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.2))

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
            (lambda () (add-to-list 'company-backends 'company-jedi))))

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
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
