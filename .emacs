
;;  Emacs functionality settings

(server-start)
(setq current-language-environment "English")

 (require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company flycheck pyvenv exec-path-from-shell html-check-frag writegood-mode pdf-tools dictionary magit markdown-mode haskell-mode js2-mode fill-column-indicator ess base16-theme)))
 '(pdf-misc-print-programm "/usr/bin/gtklp"))

(add-hook 'prog-mode-hook 'company-mode)

(require 'ido)
(ido-mode t)

(require 'mwheel)
(mouse-wheel-mode t)

(require 'fill-column-indicator)
;; Only use fill column indicator for programming modes
(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'ess-mode-hook 'fci-mode)
(setq fci-rule-color "dark slate grey")

(setq-default indent-tabs-mode nil)

;; ignore case when searching
(setq case-fold-search t)

(set-default 'truncate-lines t)

;; windmove to navigate buffers with shift
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(setq auto-save-default nil)
(setq make-backup-files nil)

(if (eq system-type 'darwin)
    (progn
      (setq mac-option-modifier 'super)
      (setq mac-command-modifier 'meta)))
(if (eq system-type 'gnu/linux)
    (progn
      (setq x-meta-keysym 'meta)
      (setq x-super-keysym 'super)))

;; Dired (from http://ergoemacs.org/emacs/emacs_dired_tips.html)

(defun dired-mode-setup ()
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'dired-mode-setup)

;; Appearence

(setq initial-scratch-message nil)
(scroll-bar-mode -1)
(blink-cursor-mode 0)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(column-number-mode)
(show-paren-mode t)

(if (display-graphic-p) 
    (load-theme 'base16-tomorrow-night t)
  nil)

;;fringe stuff
(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(require 'tool-bar)
(tool-bar-mode -1)

(require 'fringe)
(fringe-mode 30)

;; Can't get pdf-tools to compile on MacOS
(if (eq system-type 'gnu/linux)
    (pdf-tools-install))

;; visible bell is fucked up in osx el capitan, disabled for now
(setq visible-bell nil) ; default
(setq ring-bell-function 'ignore) ; disable bell ringing

;; Python

(setq
 python-shell-interpreter "ipython3"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 python-shell-interpreter-args "--simple-prompt -i")

;; Markdown

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Javascript / Web

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'html-check-frag)
(add-hook 'html-mode-hook (lambda () (html-check-frag-mode 1)))

;; ESS

(eval-after-load 'ess-site '(ess-toggle-underscore nil))

;; ESS indent styles
(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'C++ 'quiet)
            (setq ess-ask-for-ess-directory nil)
            (setq ess-first-continued-statement-offset 2)
            (setq ess-continued-statement-offset 0)))

(if (eq system-type 'gnu/linux)
    (setq inferior-R-program-name "/usr/bin/R"))

(if (eq system-type 'darwin)
    (setq inferior-R-program-name "/usr/local/bin/R"))

;; Hopefully fixes weird el-capitan bug that prevents ess from finding R
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
