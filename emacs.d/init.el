;;;;
;;;; package.el
;;;;

;; must come before configuring installed packages
(package-initialize)

;;;;
;;;; shell goodies
;;;;

(require 'shell)

(setq comint-move-point-for-matching-input 'end-of-line)

;; bind M-p and M-n to cycle only thru history items that complete
;; the characters entered so far
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key (current-local-map) (kbd "M-p")
              'comint-previous-matching-input-from-input)
            (define-key (current-local-map) (kbd "M-n")
              'comint-next-matching-input-from-input)))

(setq comint-buffer-maximum-size 100000)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(global-set-key (kbd "C-x f") 'find-file-at-point)

;; bind F1-F4 to separate numbered shell buffers
(defun myshell (arg)
  (interactive "c")
  (if (or (< arg 32) (> arg 126))
      (error "Please use printable character for shell name"))
  (let* ((buffer (get-buffer (format "*shell*<%c>" arg))))
    (if buffer
	(switch-to-buffer buffer)
      (progn
	(shell)
	(rename-buffer (format "*shell*<%c>" arg))))))

(define-key global-map [f1] (lambda () (interactive) (myshell ?1)))
(define-key global-map [f2] (lambda () (interactive) (myshell ?2)))
(define-key global-map [f3] (lambda () (interactive) (myshell ?3)))
(define-key global-map [f4] (lambda () (interactive) (myshell ?4)))

(setq explicit-bash-args '("--login" "-i")) ; for Mac

;; speed up shell output
(setq comint-scroll-show-maximum-output nil)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;;;;
;;;; tramp goodies
;;;;

(defun comint-ssh-file-name-prefix (output)
  "Set up filename completion via tramp in ssh shells.

Looks for shell commands like 'ssh -p port user@host' and sets
`comint-file-name-prefix' to '/ssh:user@host#port:'

Should be added to `comint-input-filter-functions' like so:

  (add-hook 'comint-input-filter-functions
            'comint-ssh-file-name-prefix)
  "

  (if (string-match ".*\\bssh\\b\\( +-p *\\(\\S-+\\)\\)? +\\(\\(\\(\\S-+\\)@\\)?\\(\\S-+\\)\\)" output)
      (let ((port (match-string 2 output))
	    (user-host (match-string 3 output)))
	(setq comint-file-name-prefix 
	      (concat "/ssh:" 
		      user-host
		      (if port (concat "#" port) "")
		      ":"))
	)))

(add-hook 'comint-input-filter-functions
	  'comint-ssh-file-name-prefix)

(defun comint-ssh-file-name-prefix-clear (output)
  (if (string-match "onnection to \\S-+ closed" output)
      (setq comint-file-name-prefix "")))

(add-hook 'comint-output-filter-functions
          'comint-ssh-file-name-prefix-clear)

(setq tramp-use-ssh-controlmaster-options nil)

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

(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil."
  (when pid
    (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
      (when (file-exists-p cmdline-file)
        (with-temp-buffer
          (insert-file-contents-literally cmdline-file)
          (goto-char (point-min))
          (when (search-forward "emacs" nil t)
	    pid))))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(add-hook 'auto-save-hook (lambda () (desktop-save desktop-dirname t)))

;;;;
;;;; indentation goodies
;;;;

(setq-default indent-tabs-mode nil)

(require 'dtrt-indent "~/.emacs.d/dtrt-indent.el")
(require 'dtrt-indent "~/.emacs.d/dtrt-indent-diag.el")
(dtrt-indent-global-mode 1)

(add-hook 'c-mode-hook '(lambda () (c-set-style "linux")))
(add-hook 'c++-mode-hook '(lambda () (c-set-style "linux")))

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

(setq Man-notify-method 'pushy)

(setq woman-fill-frame t)

(column-number-mode)

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
 '(package-selected-packages '(lua-mode multiple-cursors)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
