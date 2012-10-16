; .emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load Steve Purcell's super configuration bundle
(load "~/.emacs.d/init.el")

;; set personal info
(setq user-full-name "Yanming Wang")

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(color-theme-initialize)
(color-theme-dark-blue2)


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
;; set indent using tab
;(setq-default indent-tabs-mode nil)
;(setq default-tab-width 8)
;(setq tab-stop-list ())
;(loop for x downfrom 40 to 1 do
;      (setq tab-stop-list (cons (* x 4) tab-stop-list)))

;;========================================
;; 键绑定
;;========================================

;; C-z 设置记 ;;  C-z:M-x iconify-or-deiconify-frame:C-z,C-x C-z
(global-set-key (kbd "C-z") 'set-mark-command)

;;WIN+s进入Shell ;; M-x shell
(global-set-key (kbd "s-s") 'shell)
;;(define-key ctl-x-map "\M-s" 'shell)

;;WIN+space 设置标记
(global-set-key (kbd "s-SPC") 'set-mark-command)

;; set latitude and longitude
(setq calendar-latitude +39.9)
(setq calendar-longitude +116.3)
(setq calendar-location-name "Beijing")

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
;(global-set-key (kbd "<f9>") 'rename-buffer)

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

;; Java mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun linux-java-mode()
  (define-key java-mode-map [return] 'newline-and-indent)
  (interactive)
  (c-set-style "java")
  (c-toggle-auto-state)
  (c-toggle-hungry-state)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (imenu-add-menubar-index)
  (which-function-mode)
)

(add-hook 'java-mode-hook 'linux-java-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'python-mode-hook
  (lambda ()
    (set-variable 'py-indent-offset 4)
    ;(set-variable 'py-smart-indentation nil)
    (set-variable 'indent-tabs-mode nil)
    (define-key python-mode-map (kbd "RET") 'newline-and-indent)
    ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
    (smart-operator-mode-on)
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
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
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
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key mapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
(global-set-key [f1] 'goto-line);设置M-g为goto-line
;(global-set-key [f2] 'split-window-horizontally );
(global-set-key [(f2)] 'speedbar);打开speedbar
;; refresh buffer
(global-set-key (kbd "C-c r") 'revert-buffer)

;; regex replace
(global-set-key (kbd "M-#") 'query-replace-regexp)

;; binding goto-line
(global-set-key (kbd "M-g") 'goto-line)

;;-------------------------让Emacs拥有tabs-------------------
;;(require 'tabbar)
;;(setq tabbar-buffer-groups-function
;;          (lambda ()
;;            (list "All")))
;; 
;;(tabbar-mode)
;;(global-set-key [(control shift tab)] 'tabbar-backward)
;;(global-set-key [(control tab)] 'tabbar-forward)
;;(global-set-key (kbd "M-1") 'tabbar-backward)
;;(global-set-key (kbd "M-2") 'tabbar-forward)
;;
;;;;;; 设置tabbar外观
;;;; 设置默认主题: 字体, 背景和前景颜色，大小
;;(set-face-attribute 'tabbar-default nil
;;                    :family "Vera Sans YuanTi Mono"
;;                    :background "gray80"
;;                    :foreground "gray30"
;;                    :height 1.0
;;                    )
;;;; 设置左边按钮外观：外框框边大小和颜色
;;(set-face-attribute 'tabbar-button nil 
;;                    :inherit 'tabbar-default
;;                    :box '(:line-width 1 :color "gray30")
;;                    )
;;;; 设置当前tab外观：颜色，字体，外框大小和颜色
;;(set-face-attribute 'tabbar-selected nil
;;                    :inherit 'tabbar-default
;;                    :foreground "DarkGreen"
;;                    :background "LightGoldenrod"
;;                    :box '(:line-width 2 :color "DarkGoldenrod")
;;                    ;; :overline "black"
;;                    ;; :underline "black"
;;                    :weight 'bold
;;                    )
;;;; 设置非当前tab外观：外框大小和颜色
;;(set-face-attribute 'tabbar-unselected nil
;;                    :inherit 'tabbar-default
;;                    :box '(:line-width 2 :color "gray70")
;;                    )
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;;(load "desktop")
;;(desktop-load-default)
;;(desktop-read)
;;(setq desktop-save-mode t)
 
(require 'browse-kill-ring)
(global-set-key [(control c)(k)] 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;;erlang
;;(require 'erlang-eunit)
;;(load "erlang_appwiz" t nil)
;;(global-set-key [f3] 'erlang-next-error)
;;(global-set-key [C-f7] 'erlang-compile)
;;(global-set-key [f7] 'compile)
;;(global-set-key [f10] 'linum-mode)

;;distel
(add-to-list 'load-path "/usr/local/share/distel/elisp")
(require 'distel)
(distel-setup)
;; Some Erlang customizations
(add-hook 'erlang-mode-hook
        (lambda ()
              ;; when starting an Erlang shell in Emacs, default in the node name
`              (setq inferior-erlang-machine-options '("-sname" "emacs"))
              ;; add Erlang functions to an imenu menu
              (imenu-add-to-menubar "imenu")))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
        (lambda ()
              ;; add some Distel bindings to the Erlang shell
              (dolist (spec distel-shell-keys)
                  (define-key erlang-shell-mode-map (car spec) (cadr spec)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;代码跳转;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key [f8] 'semantic-ia-fast-jump)
(global-set-key [S-f8]
(lambda ()
(interactive)
(if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
(error "Semantic Bookmark ring is currently empty"))
(let* ((ring (oref semantic-mru-bookmark-ring ring))
(alist (semantic-mrub-ring-to-assoc-list ring))
(first (cdr (car alist))))
(if (semantic-equivalent-tag-p (oref first tag)
(semantic-current-tag))
(setq first (cdr (car (cdr alist)))))
(semantic-mrub-switch-tags first))))
(define-key c-mode-base-map [M-f1] 'semantic-analyze-proto-impl-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
Use ska-jump-to-register to jump back to the stored 
position." 
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
