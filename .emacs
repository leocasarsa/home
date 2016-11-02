;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done 'time)
(setq org-agenda-files (list "~/org/research.org"
                             "~/org/study.org"
			          "~/org/recreation.org"
				       "~/org/performance.org"
				            "~/org/health.org"
                             "~/org/home.org"
			          "~/org/todo.org"))
;(find-file "~/org/todo.org") ;; TODO at startup
;(org-agenda nil "a") ;; agenda at startup
(add-hook 'org-mode-hook 'leo-org-mode-hook)
(defun leo-org-mode-hook ()
  (visual-line-mode t)
  (flyspell-mode t))  


;; python mode
(defun annotate-pdb ()
(interactive)
(highlight-lines-matching-regexp "import pdb")
(highlight-lines-matching-regexp "pdb.set_trace()"))

(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
(interactive)
(py-newline-and-indent)
(insert "import ipdb; ipdb.set_trace()")
(highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
(setenv "PYTHONPATH" "/home/leo/bayeslite-venv/bin/python")


;; JavaScript:
;; Major mode js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; hook js2-mode for shell scripts via node.js
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.
  Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Packages
(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(setq package-enable-at-startup nil)
(package-initialize)


;; AUCTeX
(ensure-package-installed 'auctex)
(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Tramp
(setq tramp-default-method "ssh")

;;Open compressed files
(auto-compression-mode 1)

;; Recent files mode
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Saves window configuration
;(require 'workgroups2)

;; Elpy (python cool stuff)
(ensure-package-installed 'elpy)
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(elpy-use-ipython)
(ensure-package-installed 'ein)
(require 'ein)

;;Indent-guide
(ensure-package-installed 'indent-guide)
(require 'indent-guide)
(indent-guide-global-mode)
(setq indent-guide-recursive t)
(setq indent-guide-delay 0.1)

;;pomodoro
;(require 'pomodoro) 
;(pomodoro-add-to-mode-line)

;;git
;(require 'git)

;;cua mode
;; (cua-mode t)
;;     (setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
;;     (transient-mark-mode 1)               ;; No region when it is not highlighted
;;     (setq cua-keep-region-after-copy t) 

;; scheme REPL
;(load "xscheme")

;; iedit-mode
(ensure-package-installed 'iedit)
(require 'iedit)

;;rgrep
(define-key global-map "\C-x\C-r" 'rgrep)

;; wgrep
(ensure-package-installed 'wgrep)
(require 'wgrep)
(eval-after-load 'grepl
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

;; multiple-cursors
(ensure-package-installed 'multiple-cursors)
(require 'multiple-cursors)
;; Adds cursor to each line in an active region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; helm
;; (ensure-package-installed 'helm)
;; (require 'helm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editor settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Line numbers on the left
;; (require 'linum)
;; (line-number-mode 1)
;; (column-number-mode 1)
;; (global-linum-mode 1)

;; Line numbers globally
(global-linum-mode t)


;; Load spolsky theme
;; (setq package-enable-at-startup nil)
;; (package-initialize)
;; (load-theme 'spolsky t)

;; My key settings
(global-set-key (kbd "C-S-<iso-lefttab>") 'other-window)
(global-set-key (kbd "C-|") 'other-frame)
(setq inhibit-startup-screen t)

;; Show full file name
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Open file with dired
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))


;;Create folder with find-file
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;;Create folder with save-buffer
(defadvice save-buffer (before make-directory-maybe (&optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p (buffer-file-name))
    (let ((dir (file-name-directory (buffer-file-name))))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;; Group directories in dired
(setq dired-listing-switches "-aBhl  --group-directories-first")

;; dired reuse buffer
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
 ))

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style Undo
(global-set-key [(control z)] 'undo)

;; comment lines
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; quickly delete file and buffer
(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c d") 'delete-this-buffer-and-file)

;show-paren-mode related
(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the
        matching line in the echo area. Has no
        effect if the character before point is
        not of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	  (matching-text (and cb
			           (char-equal (char-syntax cb) ?\) )
				        (blink-matching-open))))
    (when matching-text (message matching-text))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs auto setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "0c29db826418061b40564e3351194a3d4a125d182c6ee5178c237a7364f0ff12" default)))
 '(linum-format " %7i ")
 '(org-todo-keywords (quote ((sequence "TODO(t)" "IDLE(i)" "DONE(d)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;(workgroups-mode 1) 
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
