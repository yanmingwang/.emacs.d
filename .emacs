					; .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load Steve Purcell's super configuration bundle
(load "~/.emacs.d/init.el")

;; set personal info
(setq user-full-name "Michael Wang")

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(color-theme-initialize)
(color-theme-dark-blue2)

;; set latitude and longitude
(setq calendar-latitude +39.9)
(setq calendar-longitude +116.3)
(setq calendar-location-name "Beijing")

;;显示行列号
(setq column-number-mode t)
(setq line-number-mode t)
(global-linum-mode 'linum-mode);;在左边显示行号

;; 有 toolbar
(setq tool-bar-mode t)
;; 没有 menubar
(setq menu-bar-mode nil)

;; use clipboard
(setq x-select-enable-clipboard t)

;;光标靠近鼠标指针时，让鼠标指针自动让开
(mouse-avoidance-mode 'animate)

;;(mouse-avoidance-mode 'animate)

;; 在退出 emacs 之前确认是否退出
;;(setq confirm-kill-emacs 'yes-or-no-p)

;; 设置 hippie-expand 很好用的功能。 M-x hippie-expand
(setq hippie-expand-try-functions-list
      '(
	try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-all-abbrevs
	try-expand-list3
	try-expand-line
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol))

(require 'ido)
(ido-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [f1] 'goto-line);设置M-g为goto-line
(global-set-key [(f2)] 'speedbar);打开speedbar
;; refresh buffer
(global-set-key (kbd "C-c r") 'revert-buffer)
					;(global-set-key [(f8)] 'other-window)
					;(global-set-key [(f4)] 'ibuffer)
(global-set-key [C-f4] 'kill-this-buffer)
;; regex replace
(global-set-key (kbd "M-#") 'query-replace-regexp)
;; binding goto-line
;;(global-set-key (kbd "M-g") 'goto-line)
;; C-z 设置记 ;;  C-z:M-x iconify-or-deiconify-frame:C-z,C-x C-z
(global-set-key (kbd "C-z") 'set-mark-command)
;;WIN+s进入Shell ;; M-x shell
(global-set-key (kbd "s-s") 'shell):
;;(define-key ctl-x-map "\M-s" 'shell)

;; default to better frame titles
(setq frame-title-format (concat  "%b - emacs@" (system-name)))

;; display date and time
(setq display-time-day-and-date t)
(display-time)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
(setq display-time-use-mail-icon t)
(setq display-time-interval 10)

;; highlight the holidays and birthdays
(setq mark-holidays-in-calendar t)
(setq view-calendar-holidays-initially t)

;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode) (global-font-lock-mode t))

;; utf-8 settings
(set-language-environment "utf-8")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

;; always end a file with a newline
					;(setq require-final-newline 'query)

;; set default mode, instead of fundamental-mode
(setq default-major-mode 'text-mode)

;; display images
(auto-image-file-mode t)

;; highlight the matched parentheses
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; a large kill riing
(setq kill-ring-max 200)

;; default to unified diffs
(setq diff-switches "-u")

;; type M-y to display kill-ring
(require 'browse-kill-ring nil t)
(when (featurep 'browse-kill-ring) (browse-kill-ring-default-keybindings))

;; disable generating backup files, e.g. main.c~
(setq-default make-backup-files nil)

;; backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)

;; version control
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)

;; enable these default functions
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;; dired
(setq dired-omit-extensions
      '(
	".svn/" "CVS/" ".o" "~" ".bin" ".bak" ".obj" ".map" ".ico"
	".pif" ".lnk" ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd"
	".386" ".elc" ".lof" ".glo" ".idx" ".lot" ".fmt" ".tfm"
	".class" ".lib" ".mem" ".x86f" ".sparcf" ".fasl"
	".ufsl" ".fsl" ".dxl" ".pfsl" ".dfsl" ".lo" ".la" ".gmo"
	".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
	".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"
	".idx" ".lof" ".lot" ".glo" ".blg" ".bbl" ".cps" ".fn"
	".fns" ".ky" ".kys" ".pg" ".pgs" ".tp" ".tps" ".vr" ".vrs"
	".pdb" ".ilk"))
(setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.\\|^~")
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; shell
(setq shell-file-name "/bin/bash")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(global-set-key (kbd "C-c z") 'eshell)
(global-set-key (kbd "<f9>") 'rename-buffer)

;; enable copying contents from emacs to the other programs
(when (eq window-system 'x) (setq x-select-enable-clipboard t))

;; avoid crashing emacs by pressing 'print' button on menu
(fset 'print-buffer 'ignore)
(setq lpr-command "")
(setq printer-name "")

;; wider context view
(setq scroll-margin 3 scroll-conservatively 10000)
(mouse-avoidance-mode 'animate)
(setq-default ispell-program-name "aspell")
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default show-trailing-whitespace nil)
(setq default-fill-column 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Java mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linux-java-mode()
  (define-key java-mode-map [return] 'newline-and-indent)
  (interactive)
  (c-set-style "java")
  (c-toggle-auto-state)
  (c-toggle-hungry-state)
  (setq c-basic-offset 2)
  (setq c-indent-level 2)
  (imenu-add-menubar-index)
  (which-function-mode)
  )
(add-hook 'java-mode-hook 'linux-java-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
	  (lambda ()
	    (set-variable 'py-indent-offset 2)
					;(set-variable 'py-smart-indentation nil)
	    (set-variable 'indent-tabs-mode nil)
	    (define-key python-mode-map (kbd "RET") 'newline-and-indent)
					;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
					;(smart-operator-mode-on)
	    ))

;; pdb setup, note the python version
(setq pdb-path '/usr/local/lib/python2.7/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
			    (file-name-nondirectory buffer-file-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)

(add-hook 'c-mode-hook 'linux-c-mode)
(add-hook 'c++-mode-hook 'linux-cpp-mode)
(setq imenu-sort-function 'imenu--sort-by-name)

(defun linux-c-mode()
  (define-key c-mode-map [return] 'newline-and-indent)
  (define-key c++-mode-map [(control c) (c)] 'compile)
  (interactive)
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 2)
  (setq c-indent-level 2)
  ;;(c-set-style "K&R")
  ;;(c-toggle-auto-state)
  ;;(c-toggle-hungry-state)
  (imenu-add-menubar-index)
  (which-function-mode))

(defun linux-cpp-mode()
  (define-key c++-mode-map [return] 'newline-and-indent)
  (define-key c++-mode-map [(control c) (c)] 'compile)
  (interactive)
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 2)
  (setq c-indent-level 2)
  ;;(c-set-style "K&R")
  ;;(c-toggle-auto-state)
  ;;(c-toggle-hungry-state)
  (imenu-add-menubar-index)
  (which-function-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enhancements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yasnippet
(require 'yasnippet)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
(define-key ac-completing-map "\M-n" 'ac-next)
(define-key ac-completing-map "\M-p" 'ac-previous)
(setq ac-auto-start 2)
(setq ac-dwim t)
(define-key ac-mode-map (kbd "M-,") 'auto-complete)

(setq hippie-expand-try-functions-list
      '(try-expand-line
	try-expand-dabbrev
	try-expand-line-all-buffers
	try-expand-list
	try-expand-list-all-buffers
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name
	try-complete-file-name-partially
	try-complete-lisp-symbol
	try-complete-lisp-symbol-partially
	try-expand-whole-kill))

(global-set-key (kbd "M-;") 'dabbrev-expand)
(global-set-key (kbd "M-'") 'hippie-expand)
(global-set-key (kbd "M-/") 'wd-hippie-expand-filename)

(defun wd-hippie-expand-filename ()
  (interactive)
  (let ((hippie-expand-try-functions-list
	 '(try-complete-file-name try-complete-file-name-partially)))
    (call-interactively 'hippie-expand)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; erlang mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

;;(setq erlang-root-dir "/usr/local/local/lib/erlang")
;;(add-to-list 'exec-path "/usr/local//lib/erlang/bin")
;;(setq erlang-man-root-dir "/usr/local/lib/erlang/man")
(setq erlang-root-dir "/usr/local/lib/erlang")
(setq load-path (cons "/usr/local/lib/erlang/lib/tools-2.6.7/emacs" load-path))
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
(require 'erlang-start)
(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default in the node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; customize keys
  (local-set-key [return] 'newline-and-indent)
  )
;; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defconst distel-shell-keys
  '(("\C-\M-i"	 erl-complete)
    ("\M-?"	 erl-complete)
    ("\M-."	 erl-find-source-under-point)
    ("\M-,"	 erl-find-source-unwind)
    ("\M-*"	 erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
	  (lambda ()
	    ;; add some Distel bindings to the Erlang shell
	    (dolist (spec distel-shell-keys)
	      (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;注释/反注释
(defun qiang-comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))
(global-set-key "\M-;" 'qiang-comment-dwim-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy region or whole line
(global-set-key "\M-w"
		(lambda ()
		  (interactive)
		  (if mark-active
		      (kill-ring-save (region-beginning)
				      (region-end))
		    (progn
		      (kill-ring-save (line-beginning-position)
				      (line-end-position))
		      (message "copied line")))))

;; kill region or whole line
(global-set-key "\C-w"
		(lambda ()
		  (interactive)

		  (if mark-active
		      (kill-region (region-beginning)
				   (region-end))
		    (progn
		      (kill-region (line-beginning-position)
				   (line-end-position))
		      (message "killed line")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  tag and jump
(global-set-key [(control ?\.)] 'ska-point-to-register)
(global-set-key [(control ?\,)] 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))

(define-key global-map (kbd "C-c a") 'wy-go-to-char)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;remove useless spaces when save file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add save buffer hook
(defun reed_update_file()
  "Convert spaces to tabs, and remove useless spaces"
  (interactive)

  ;; Remove useless spaces
  (edit-picture)
  (picture-mode-exit)

  ;; Convert spaces to tabs
  (tabify (point-min) (point-max))

  ;; Save buffer
  ;;(basic-save-buffer)
  )

;; Add-hook to automate the task when we save files
(add-hook 'write-file-hooks 'reed_update_file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;格式化整个文件函数
;; (defun indent-whole ()
;;   (interactive)
;;   (indent-region (point-min) (point-max))
;;   (message "format successfully"))
;; ;;绑定到F7键
;; (global-set-key [f7] 'indent-whole)
