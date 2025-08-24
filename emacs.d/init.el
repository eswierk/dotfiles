;;;;
;;;; package.el
;;;;

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

(require 'vterm)

(setq vterm-max-scrollback 100000)

;; bind F1-F4 to separate numbered shell buffers
(defun myshell (arg)
  (interactive "c")
  (if (or (< arg 32) (> arg 126))
      (error "Please use printable character for shell name"))
  (let* ((buffer (get-buffer (format "*shell*<%c>" arg))))
    (if buffer
	(switch-to-buffer buffer)
      (progn
	(vterm)
	(rename-buffer (format "*shell*<%c>" arg))))))

(define-key global-map [f1] (lambda () (interactive) (myshell ?1)))
(define-key global-map [f2] (lambda () (interactive) (myshell ?2)))
(define-key global-map [f3] (lambda () (interactive) (myshell ?3)))
(define-key global-map [f4] (lambda () (interactive) (myshell ?4)))

(define-key vterm-mode-map [f1] (lambda () (interactive) (myshell ?1)))
(define-key vterm-mode-map [f2] (lambda () (interactive) (myshell ?2)))
(define-key vterm-mode-map [f3] (lambda () (interactive) (myshell ?3)))
(define-key vterm-mode-map [f4] (lambda () (interactive) (myshell ?4)))

(define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
(define-key vterm-mode-map [down-mouse-1] #'vterm-copy-mode)

(define-key vterm-copy-mode-map [return] #'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "RET") #'vterm-copy-mode)

(setq vterm-enable-manipulate-selection-data-by-osc52 t)

;; hint to the remote terminal that it's running inside emacs-vterm
(setq vterm-term-environment-variable "xterm-color")

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
   '(dtrt-indent gnu-elpa-keyring-update lua-mode magit markdown-mode multiple-cursors swift-mode vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
