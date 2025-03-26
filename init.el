(require 'package)                                          ; activate packagesnix-channel --list
'(package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa" . "https://melpa.org/packages/")))


;;(package-initialize)                                        ; initialize package facility


(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(use-package web-mode
  :ensure t)


;;;;;;;;;;;;;;;;;;; some c stuff which I just chucked in, hopefully good :)

(use-package eglot)


(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package lsp-ui
  :commands lsp-ui-mode
  :ensure t)
;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq lsp-prefer-flymake nil)
;;   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((c-mode c++-mode objc-mode) .
;;          (lambda () (require 'ccls) (lsp))))
(use-package flycheck
  :ensure t)
(use-package yasnippet
  :ensure t
  :config (yas-global-mode))
(use-package which-key
  :ensure t
  :config (which-key-mode))
(use-package helm-lsp
  :ensure t)
(use-package helm
  :ensure t
  :config (helm-mode))
(use-package lsp-treemacs
  :ensure t)


;;; This will enable emacs to compile a simple cpp single file without any makefile by just pressing [f9] key
(defun code-compile()
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


(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

(use-package company :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; End of stuff which may be good

;;; golang

(require 'company)
(require 'yasnippet)

(require 'go-mode)
(require 'eglot)
(add-hook 'go-mode-hook 'eglot-ensure)

;; Optional: install eglot-format-buffer as a save hook.
;; The depth of -10 places this before eglot's willSave notification,
;; so that that notification reports the actual contents that will be saved.
(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))
(add-hook 'go-mode-hook #'eglot-format-buffer-before-save)
;;



 ;;(guix-emacs-autoload-packages)

 ;; in theory hide the title bar
 ;;(add-to-list 'default-frame-alist '(undecorated . t))

 (setq my-packages
       '(go-mode lua-mode modus-themes ef-themes magit catppuccin-theme lsp-mode)
       )

 (setq-default dired-listing-switches "-alh")

;; TBD - 
 ;; (unless package-archive-contents    ; unless packages are not available locally, dont refresh package archives
 ;;   (package-refresh-contents))       ; refreshing package contents is time-consuming and should be done on demand

 (dolist (pkg my-packages)          
   (unless (package-installed-p pkg)
     (package-install pkg)))        

 (setq scheme-program-name "racket")

 (setq inhibit-startup-message t)  ; Comment at end of line!
 (setq ring-bell-function 'ignore)

 ;; Turn off some unneeded UI elements
 ;;(menu-bar-mode -1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1)
 ;; Save what you enter into minibuffer prompts
 (setq history-length 25)
 (savehist-mode 1)
 ;; Revert buffers when the underlying file has changed
 (global-auto-revert-mode 1)
 ;; Revert buffers when the underlying file has changed
 (global-auto-revert-mode 1)

 ;; Display line numbers in every buffer
 (global-display-line-numbers-mode 1)

 ;;(setq modus-themes-common-palette-overrides '((border-mode-line-active
 ;;    unspecified) (border-mode-line-inactive unspecified)))

 (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha


 ;; Load the Modus Vivendi dark theme
					;(load-theme 'modus-vivendi t)
 (load-theme 'catppuccin :no-confirm)



 (add-to-list 'auto-mode-alist '("\\.script\\'" . lua-mode))

 (setq-default tab-width 4)


 (defun my-set-tab-mode ()
   (when (and (stringp buffer-file-name)
	      (string-match "\\.cat\\'" buffer-file-name))
     (insert "OK")
     (orgtbl-mode)))

 (add-hook 'find-file-hook 'my-set-tab-mode)

 (setq tab-always-indent 'complete)
 (add-to-list 'completion-styles 'initials t)

 ;; go Stuff

 ;; (require 'lsp-mode)
 ;; (require 'go-mode)

 ;; (add-hook 'go-mode-hook 'lsp-deferred)
 ;; (add-hook 'go-mode-hook 'subword-mode)
 ;; (add-hook 'before-save-hook 'gofmt-before-save)

 ;; (add-hook 'go-mode-hook (lambda ()
 ;;                           (setq tab-width 4)
 ;;                           (flycheck-add-next-checker 'lsp 'go-vet)
 ;;                           (flycheck-add-next-checker 'lsp 'go-staticcheck)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(ccls company lsp-mode lsp-ui web-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
