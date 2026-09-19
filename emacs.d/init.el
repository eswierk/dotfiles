;;; -*- lexical-binding: t; -*-

;;;;
;;;; package.el
;;;;

;; work around a stale Darwin-major → macOS-version mapping baked into this
;; build's libgccjit: it computes -mmacosx-version-min from `uname -r` using
;; a formula that predates the macOS 15->26 version jump, producing an
;; invalid target (e.g. 18.0) and breaking native compilation of any not-yet
;; -cached .el file. Appending the correct value (last flag wins) overrides it.
(require 'comp)
(when (eq system-type 'darwin)
  (add-to-list 'native-comp-driver-options
               (format "-mmacosx-version-min=%s" (string-trim (shell-command-to-string "sw_vers -productVersion")))
               t))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; must come before configuring installed packages
(package-initialize)

;;;;
;;;; shell goodies
;;;;

(use-package ghostel
  :init
  ;; must be set before terminals are created
  (setq ghostel-max-scrollback (ash 150 20) ; 150 MB ≈ ~100k lines
        ghostel-enable-osc52 t
        ghostel-tramp-shell-integration t
        ;; prevent emacs libraries like libgccjit from interfering
        ;; with builds in subshells
        ghostel-environment '("LIBRARY_PATH="))
  :bind (("C-x m" . ghostel))
  :config
  ;; F1-F4: separate numbered shell buffers (global + in semi-char mode)
  (let ((map (make-sparse-keymap)))
    (dotimes (i 4)
      (let ((key (vector (intern (format "f%d" (1+ i)))))
            (name (number-to-string (1+ i))))
        (define-key global-map key
          (lambda () (interactive) (my-ghostel-shell name)))
        (define-key ghostel-semi-char-mode-map key
          (lambda () (interactive) (my-ghostel-shell name)))))))

(defun my-ghostel-shell (name)
  "Switch to or create the ghostel shell buffer named *shell*<NAME>."
  (interactive)
  (let* ((bufname (format "*shell*<%s>" name))
         (buffer (get-buffer bufname)))
    (switch-to-buffer (or buffer (ghostel-create bufname)))))

(defun my-ghostel-set-dir (user host dir)
  (setq default-directory
        (format "/ssh:%s@%s:%s" user host dir)))

(add-to-list 'ghostel-eval-cmds
             '("set-dir" my-ghostel-set-dir))

;;;;
;;;; tramp goodies
;;;;

(setq tramp-use-ssh-controlmaster-options nil)
(setq tramp-allow-unsafe-temporary-files t)

;;;;
;;;; buffer goodies
;;;;

(require 'ido)

(ido-mode t)

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

;;;;
;;;; emacsclient goodies
;;;;

(server-start)

(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

;;;;
;;;; desktop goodies
;;;;

(require 'desktop)

(desktop-save-mode t)

(setq desktop-restore-eager 10)
(setq desktop-save t)

;; auto-save the desktop whenever file buffers are auto-saved
(add-hook 'auto-save-hook (lambda () (desktop-save desktop-dirname t)))

;;;;
;;;; indentation goodies
;;;;

(setq-default indent-tabs-mode nil)

(require 'dtrt-indent)
(dtrt-indent-global-mode 1)
;; set tab-width to match c-basic-offset, to align improperly
;; tab-indented lines in supposedly space-indented files
(add-to-list 'dtrt-indent-hook-generic-mapping-list
             '(c-buffer-is-cc-mode tab-width))

(add-hook 'c-mode-hook '(lambda () (c-set-style "linux")))
(add-hook 'c++-mode-hook '(lambda () (c-set-style "linux")))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.tightbeam\\'" . swift-mode))

(setq-default fill-column 100)

;;;;
;;;; python goodies
;;;;

(add-to-list 'interpreter-mode-alist '("python2" . python-mode))
(add-to-list 'interpreter-mode-alist '("python3" . python-mode))

;;;;
;;;; mail goodies
;;;;

(require 'smtpmail)
(setq send-mail-function 'smtpmail-send-it)
(setq user-full-name "Edward Swierk")
(setq user-mail-address "eswierk@gmail.com")
(setq smtpmail-smtp-server "smtp.gmail.com")
(setq smtpmail-smtp-service 587)

;;;;
;;;; claude
;;;;

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :after ghostel
  :bind ("C-c C-'" . claude-code-ide-menu)
  :init
  ;; evaluated before the deferred package loads
  (setq claude-code-ide-use-side-window nil
        claude-code-ide-terminal-backend 'ghostel)
  :config
  (claude-code-ide-emacs-tools-setup))

;;;;
;;;; eglot
;;;;

(use-package eglot
  ; ...
  :bind (:map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l r" . eglot-rename)
	      ("C-c l h" . eldoc)
	      ("C-c l f" . eglot-format)
	      ("C-c l F" . eglot-format-buffer)
	      ("C-c l d" . xref-find-definitions-at-mouse)
	      ;; sometimes ionide acts up
	      ("C-c l R" . eglot-reconnect)))

;;;;
;;;; other goodies
;;;;

(global-set-key (kbd "C-x f") 'find-file-at-point)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(setq inhibit-startup-message t)

(delete-other-windows)
(split-window-horizontally)

(setq split-height-threshold nil)
(setq split-width-threshold nil)

(menu-bar-mode 0)

(tool-bar-mode 0)

(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "coral"))

(xterm-mouse-mode 1)

(setq Man-notify-method 'pushy)

(column-number-mode)

(setq hexl-bits 8)

(require 'git-commit)

; Display line number rather than ?? even when buffer contains long
; lines
(setq line-number-display-limit-width 10000)

; See https://emacs.stackexchange.com/a/59134 for info on
; packages-selected-packages
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(claude-code-ide dash dtrt-indent ghostel git-commit gnu-elpa-keyring-update lua-mode magit
                     markdown-mode multiple-cursors swift-mode))
 '(package-vc-selected-packages
   '((claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
