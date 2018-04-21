;; INSTALL PACKAGES
;; --------------------------------------
; Manually load package instead of waiting until after init.el is loaded
(package-initialize)
; Disable loading package again after init.el
(setq package-enable-at-startup nil)

; Enable "package", for installing packages
; Add some common package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

; Use "package" to install "use-package", a better package management and config system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

; Make OS shell path available in emacs exec path
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-copy-env "PATH"))

;; VISUAL CUSTOMIZATION
;; --------------------------------------
(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'manoj-dark t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; Show full file name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

; Create a 80-character line marker
; With a work-around so that fill-column-indicator works with company mode
; https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
(use-package fill-column-indicator
  :ensure t
  :config
  (setq fci-rule-column 80)
  (add-hook 'prog-mode-hook 'fci-mode))

;; Modeline similar to spacemacs
(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme))

;; Dims other buffers
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; REFACTORING
;; --------------------------------------

;; iedit-mode
(use-package iedit)

;;rgrep
(define-key global-map "\C-x\C-r" 'rgrep)

;; wgrep
(use-package wgrep)
(eval-after-load 'grepl
  '(define-key grep-mode-map
     (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(eval-after-load 'wgrep
  '(define-key grep-mode-map
     (kbd "C-c C-c") 'wgrep-finish-edit))

;; multiple-cursors
(use-package multiple-cursors)
;Adds cursor to each line in an active region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; MAJOR MODES
;; --------------------------------------

; Set up auctex for Latex in Emacs
; Point auctex to my central .bib file
(use-package tex
  :ensure auctex
  :config
  (setq Tex-auto-save t)
  (setq Tex-parse-self t)
  (setq TeX-save-query nil)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-default-bibliography '("/home/anh/texmf/bibtex/bib/local/library.bib")))

; Set up elpy for Python in Emacs
(use-package elpy
  :ensure t
  :pin elpy
  :config
  (elpy-enable)
  ;; Enable elpy in a Python mode
  (add-hook 'python-mode-hook 'elpy-mode)
  (setq elpy-rpc-backend "jedi")
  ;; Open the Python shell in a buffer after sending code to it
  (add-hook 'inferior-python-mode-hook 'python-shell-switch-to-shell)
  ;; Use IPython as the default shell, with a workaround to accommodate IPython 5
  ;; https://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "--simple-prompt -i")
  ;; Enable pyvenv, which manages Python virtual environments
  (pyvenv-mode 1)
  ;; Tell Python debugger (pdb) to use the current virtual environment
  ;; https://emacs.stackexchange.com/questions/17808/enable-python-pdb-on-emacs-with-virtualenv
  (setq gud-pdb-command-name "python -m pdb "))

; Set up projectile, i.e. package management + helm, i.e. autocomplete
; Tutorial - recommended: https://tuhdo.github.io/helm-projectile.html
;; (use-package projectile
;;   :ensure t
;;   :config
;;   (projectile-global-mode)
;;   (setq projectile-completion-system 'helm)
;;   (setq projectile-switch-project-action 'helm-projectile))

;; (use-package helm-projectile
;;   :ensure t
;;   :config
;;   (helm-projectile-on))

;; (use-package helm-config
;;   :ensure helm
;;   :config
;;   (helm-mode 1)
;;   (global-set-key (kbd "M-x") 'helm-M-x)
;;   (global-set-key (kbd "C-x C-f") 'helm-find-files))

; Set up company, i.e. code autocomplete
(use-package company
  :ensure t
  :config
  ;; Enable company mode everywhere
  (add-hook 'after-init-hook 'global-company-mode)
  ;; Set up TAB to manually trigger autocomplete menu
  (define-key company-mode-map (kbd "<backtab>") 'company-complete)
  (define-key company-active-map (kbd "<backtab>") 'company-complete-common)
  ;; Set up M-h to see the documentation for items on the autocomplete menu
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer))

; Set up company-jedi, i.e. tell elpy to use company autocomplete backend
(use-package company-jedi
  :ensure t
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

;; ; Set up ESS, i.e. Statistics in Emacs, R, Stata, etc.
;; (use-package ess-site
;;   :ensure ess
;;   :config
;;   (ess-toggle-underscore nil) ; http://stackoverflow.com/questions/2531372/how-to-stop-emacs-from-replacing-underbar-with-in-ess-mode
;;   (setq ess-fancy-comments nil) ; http://stackoverflow.com/questions/780796/emacs-ess-mode-tabbing-for-comment-region
;;   ; Make ESS use RStudio's indenting style
;;   (add-hook 'ess-mode-hook (lambda() (ess-set-style 'RStudio)))
;;   ; Make ESS use more horizontal screen
;;   ; http://stackoverflow.com/questions/12520543/how-do-i-get-my-r-buffer-in-emacs-to-occupy-more-horizontal-space
;;   (add-hook 'ess-R-post-run-hook 'ess-execute-screen-options)
;;   (define-key inferior-ess-mode-map "\C-cw" 'ess-execute-screen-options)
;;   ; Add path to Stata to Emacs' exec-path so that Stata can be found
;;   (setq exec-path (append exec-path '("/usr/local/stata14"))))

; Set up markdown in Emacs
; Tutorial: http://jblevins.org/projects/markdown-mode/
(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(add-hook 'text-mode-hook (lambda() (flyspell-mode 1)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

; C-n add new lines at the end of buffer
(setq next-line-add-newlines t)
; open emacs full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
; Make Emacs highlight paired parentheses
(show-paren-mode 1)


;; INTERFACE
;; --------------------------------------
;; Change window and frame
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-window)
(global-set-key (kbd "C-|") 'other-frame)

;; Delete trailing whitespace before saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package page-break-lines
  :ensure t)

(recentf-mode 1) ; keep a list of recently opened files
;; set F7 to list recently opened file
(global-set-key (kbd "<f7>") 'recentf-open-files)

;; load new loading page
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; ;; ido-mode
;; (ido-mode 1)

;; No tabs
(setq-default indent-tabs-mode nil)

(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; Directory tree
(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; icons for directory tree
(use-package all-the-icons
  :ensure t)
;; Run the following the first time
;; :config
;; (all-the-icons-install-fonts))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;;   ;; (global-set-key "\C-s" 'swiper)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   ;; (global-set-key (kbd "C-c g") 'counsel-git)
;;   ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   ;; (global-set-key (kbd "C-c k") 'counsel-ag)
;;   ;; (global-set-key (kbd "C-x l") 'counsel-locate)
;;   ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; EMACS AUTOMATIC CUSTOMIZATION
;; --------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-method (quote synctex))
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(custom-safe-themes
   (quote
    ("4e21fb654406f11ab2a628c47c1cbe53bab645d32f2c807ee2295436f09103c6" "5715d3b4b071d33af95e9ded99a450aad674e308abb06442a094652a33507cd2" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(inferior-STA-program-name "stata-se")
 '(package-selected-packages
   (quote
    (counsel swiper neotree doom-themes dimmer spaceline spaceline-config pandoc-mode ess company-jedi helm-projectile projectile auctex exec-path-from-shell use-package wgrep sql-indent protobuf-mode pdf-tools org-journal multiple-cursors material-theme markdown-mode lua-mode iedit helm flycheck fill-column-indicator elpy default-text-scale better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
