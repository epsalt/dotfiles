
;;  Emacs functionality settings
(setq current-language-environment "English")

 (require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"])
 '(ansi-term-color-vector
   [unspecified "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#81a2be" "#c5c8c6"])
 '(package-selected-packages
   (quote
    (company-jedi company flycheck pyvenv exec-path-from-shell html-check-frag writegood-mode pdf-tools dictionary magit markdown-mode haskell-mode js2-mode fill-column-indicator base16-theme)))
 '(python-shell-interpreter "python3"))

(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

(require 'ido)
(ido-mode t)

(require 'mwheel)
(mouse-wheel-mode t)

(require 'fill-column-indicator)
;; Only use fill column indicator for programming modes
(add-hook 'prog-mode-hook 'fci-mode)
(setq fci-rule-color "dark slate grey")

(setq-default indent-tabs-mode nil)
(setq case-fold-search t)
(set-default 'truncate-lines t)
(setq auto-save-default nil)
(setq make-backup-files nil)

;; windmove to navigate buffers with shift
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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

(defun my/load-theme (frame)
  (select-frame frame)
  (load-theme 'base16-tomorrow-night t))
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/load-theme)
  (load-theme 'base16-tomorrow-night t))

(require 'tool-bar)
(tool-bar-mode -1)

(require 'fringe)
(fringe-mode 30)
(set-face-attribute 'fringe nil :background nil)

;; Can't get pdf-tools to compile on MacOS
(if (eq system-type 'gnu/linux)
    (pdf-tools-install))

;; visible bell is fucked up in osx el capitan, disabled for now
(setq visible-bell nil) ; default
(setq ring-bell-function 'ignore) ; disable bell ringing

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(defun my/python-mode-hook ()
   (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "nil" :foreground "nil")))))
(put 'erase-buffer 'disabled nil)

; This fringe is going to be the death of me
(if (eq system-type 'darwin)
    (set-face-attribute 'fringe nil :background nil))
