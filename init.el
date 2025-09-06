;; Initialize package system
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu-devel" . "https://elpa.gnu.org/devel/")))

;; Start server if not running
(load "server")
(unless (server-running-p) (server-start))

;; Install use-package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(eval-when-compile
  (require 'use-package))

;; UI Configuration
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)

;; Turn off UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; History and auto-revert
(setq history-length 25)
(savehist-mode 1)
(global-auto-revert-mode 1)

;; Line numbers
(global-display-line-numbers-mode 1)

;; Theme
;;(load-theme 'ef-trio-dark t)
(load-theme 'leuven-dark t)


;; Package configuration
(use-package web-mode
  :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Font locking
(setq font-lock-highlight-level 4)
(setq treesit-font-lock-level 4)
(setq font-lock-maximum-decoration 4)

;; LSP and completion
;; (use-package eglot
;;   :ensure t
;;   :config
;;   (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
;;   (add-hook 'c-mode-hook 'eglot-ensure)
;;   (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package helm-lsp
  :ensure t)

(use-package lsp-treemacs
  :ensure t)

;; Programming modes
(use-package go-mode
  :ensure t
  :hook (go-mode . (lambda ()
                     (eglot-ensure)
                     (setq tab-width 4)
                     (add-hook 'before-save-hook #'eglot-format-buffer -10 t))))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . (lambda ()
                       (setq indent-tabs-mode nil)
                       (prettify-symbols-mode))))

(use-package lua-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.script\\'" . lua-mode)))

;; Compilation
(defun code-compile ()
  (interactive)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -o %s %s"
                   (if (equal (file-name-extension file) "cpp") "g++" "gcc")
                   (file-name-sans-extension file)
                   file)))
    (compile compile-command)))

(global-set-key [f9] 'code-compile)

;; General settings
(setq scheme-program-name "racket")
(setq-default dired-listing-switches "-alh")
(setq-default tab-width 4)
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

 (which-key-mode)
 (add-hook 'c-mode-hook 'lsp)

(require 'lsp-ui)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-position 'right) ; This puts it on the right side
(lsp-ui-sideline-mode t)
(put 'scroll-left 'disabled nil)

;; Let's run 8 checks at once instead.
(setq flymake-max-parallel-syntax-checks 8)
;; Prefer minibuffer for documentation

;; Completely disable hover functionality
;(setq eglot-extra-doc-mode nil)
;(add-to-list 'eglot-ignored-server-capabilities :hoverProvider)

(setq gdb-debuginfod-enable-setting nil)
(setq gdb-many-windows t)
(setq gdb-show-main t)
;(setq company-tooltip-limit 10)          ; Show 10 candidates max

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  
  ;; Disable the documentation buffer
  (setq eglot-extra-doc-mode nil)
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)
  ;; Optional: disable hover if you don't want any documentation
  ;; (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)
  )
