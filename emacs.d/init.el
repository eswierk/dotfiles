;;;;
;;;; shell goodies
;;;;

(require 'shell)

;; bind M-p and M-n to cycle only thru history items that complete
;; the characters entered so far
(add-hook 'comint-mode-hook
          (lambda ()
            (define-key (current-local-map) (kbd "M-p")
              'comint-previous-matching-input-from-input)
            (define-key (current-local-map) (kbd "M-n")
              'comint-next-matching-input-from-input)))

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

;;;;
;;;; buffer goodies
;;;;

(require 'iswitchb)

;; when switching buffers with C-xb, bind C-p and C-n to cycle thru
;; buffers matching the characters entered so far
(add-hook 'iswitchb-define-mode-map-hook
	  (lambda ()
	    (define-key iswitchb-mode-map (kbd "C-p")
	      'iswitchb-prev-match)
	    (define-key iswitchb-mode-map (kbd "C-n")
	      'iswitchb-next-match)))

(iswitchb-mode t)

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
;;;; other goodies
;;;;

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(setq inhibit-startup-message t)

(delete-other-windows)
(split-window-horizontally)

(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(add-to-list 'default-frame-alist '(foreground-color . "white"))
(add-to-list 'default-frame-alist '(background-color . "black"))
(add-to-list 'default-frame-alist '(cursor-color . "coral"))

(setq Man-notify-method 'pushy)

(setq split-height-threshold nil)
(setq split-width-threshold nil)
