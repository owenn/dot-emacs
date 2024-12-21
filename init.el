(require 'package)                                          ; activate packagesnix-channel --list
'(package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("nongnu" . "https://elpa.nongnu.org/nongnu/")
    ("melpa" . "https://melpa.org/packages/")))


(package-initialize)                                        ; initialize package facility


(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)


;;(add-to-list 'load-path "/home/owenn/.guix-profile/share/emacs/site-lisp")

;;(guix-emacs-autoload-packages)

;; in theory hide the title bar
;;(add-to-list 'default-frame-alist '(undecorated . t))

(setq my-packages
      '(go-mode lua-mode modus-themes ef-themes magit catppuccin-theme lsp-mode)
      )

(setq-default dired-listing-switches "-alh")

(unless package-archive-contents    ; unless packages are not available locally, dont refresh package archives
  (package-refresh-contents))       ; refreshing package contents is time-consuming and should be done on demand

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

(require 'lsp-mode)
(require 'go-mode)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)
                          (flycheck-add-next-checker 'lsp 'go-vet)
                          (flycheck-add-next-checker 'lsp 'go-staticcheck)))


