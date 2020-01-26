;; wbaer init.el
;; version 2020.01.26


(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The giant turtle on which the world rests.")


;; Packaging

;; add folders in packages to path
 (let ((default-directory (format "%s/%s" user-emacs-directory "packages")))
       (normal-top-level-add-subdirs-to-load-path))

;; add themes to path
(add-to-list 'custom-theme-load-path (format "%s/%s" user-emacs-directory "themes"))

(require 'init-packages)

;; Constants / Variables

(defconst *spell-check-support-enabled* nil)
(defconst *on-mac* (eq system-type 'darwin))
(defconst *on-windows* (eq system-type 'windows-nt))
(defconst *on-linux* (eq system-type 'gnu/linux))
(defconst *is-carbon-emacs* (eq window-system 'mac))
(defconst *is-cocoa-emacs* (and *on-mac* (eq window-system 'ns)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(coffee-args-compile (quote ("-c" "--bare")))
 '(coffee-tab-width 4)
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("2bd34d7aae55fd93e64b100826ba24ff20b8551123e22d9649c6641c3d0b5ba1" "ea3e38637c95b0c8acd035a397d98fa5b88187246ecb8bc31e7546fc4438f715" "56d84ae2d0dd0b28984201f2aa754b2fb2cf95019be681a5480cf4aba93b668d" "7ee2793c5afbf5e1fc001cf02fde1954b5f42b8a5b66cf4179682448099f0549" "a0664d4aec41442ba0db3b7bebcb7b5323617aa06e9b4ae6100347c38c352851" "6ed61522770067a63d7cfe797bede622fa69c975dd0882c7cb706e2ddb464a32" "72e42ba8f32bbf4a0b30c4c25f378289058efb1790dbd9a3e7c3d9666cc7fee4" "7122873f9ac192e4f2cfafe1679fe6b3db658ac64593efe4bc10c52d7573c6c1" "02548a935d1658d549554e497521fce413a01463b21652f62cfdfc47a9eb0050" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "9eb7ffd71418f8336284e45dbe3088c4eae065530c0613301d2580374629a202" "e2a67a7143a2e7b9f72b1091112afb041ab25ae20931c9a1288db23bca24449b" "f1c3c5084b5f2d20742ed55ecd8ee84a3fd55aa4dfdbcf0430491846e1e8add7" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "967c58175840fcea30b56f2a5a326b232d4939393bed59339d21e46cf4798ecf" "d1a574d57027c2bfadde6982455dfce8d27ced3ae4747c1c0313f95d23e96713" default)))
 '(flyspell-issue-message-flag nil)
 '(fringe-mode 6 nil (fringe))
 '(ispell-dictionary "british")
 '(ispell-list-command "--list")
 '(ispell-program-name
   (cond
    (*on-linux* "aspell")
    (*on-windows* "aspell")
    (*on-mac* "/usr/local/bin/aspell")))
 '(package-selected-packages
   (quote
    (org evil flycheck-swift swift-mode git-gitter yasnippet yaml-mode web-mode undo-tree transpose-frame sr-speedbar s rust-mode rainbow-mode rainbow-delimiters projectile powerline nyan-mode markdown-mode magit js2-mode jenkins jedi htmlize git-gutter-fringe git-gutter-fringe+ flycheck counsel company-jedi))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#ffffffffffff"))))
 '(company-scrollbar-fg ((t (:background "#ffffffffffff"))))
 '(company-tooltip ((t (:inherit default :background "#ffffffffffff"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(mode-line ((t (:foreground "#f9f9f9" :background "#4272b4" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#4272b4" :box nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "gold"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1")))))

;; On OSX it won't find flake8 otherwise
(if *on-mac*
    (setq exec-path (append exec-path '("/usr/local/bin")))
)

;; General Settings

;; no start-up message - can mess with default directory settings
(setq inhibit-startup-message t)

;; Shut up on the warnings
(setq warning-minimum-level :emergency)

;; default directory
(setq default-directory "~/")

;; line numbers in left margin
(global-linum-mode t)
;; delay linum update
(setq linum-delay t)
(setq scroll-step 1)
;; set value above 100 to ensure redisplay never recenters point
(setq scroll-conservatively 10000)

;; no toolbar
(tool-bar-mode -1)

;; no scrollbar
(scroll-bar-mode -1)

;; current line highlight
(global-hl-line-mode 1)

;; window settings
(if (window-system)  (set-frame-size (selected-frame) 180 52))

;; Sentences to end with single space
(set-default 'sentence-end-double-space nil)

;; Keep quiet
(setq ring-bell-function 'ignore)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Use async dired
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Back-up version controlled files
(setq vc-make-backup-files t)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; enable upper / lower case region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; highlight keywords
(add-hook 'prog-mode-hook (lambda ()
        (font-lock-add-keywords nil
            '(("\\<\\(FIXME\\|TODO\\|BUG\\|XXX\\|todo\\|xxx\\):" 1 font-lock-warning-face t)))
    )
)

;;highlight matching parentheses
(defun matching-parens ()
    (setq show-paren-delay 0)
    (setq show-paren-style 'parenthesis)
    (make-variable-buffer-local 'show-paren-mode)
    (show-paren-mode 1)
)
(add-hook 'prog-mode-hook 'matching-parens)

(mapc (lambda (ext)
        (push ext completion-ignored-extensions))
      '(
        ".exe" ".obj" ".class" ".dll" ".pyc" ".rbc" ".bak" ".so" ".swp" ".jar" ".desktop"
        ".doc" ".xls" ".odt" ".ods" ".ots" ".xlsx"
        ".zip" ".rar" ".tar.gz" ".tar"
        ".mp3" ".wav" ".mp4" ".mpeg" ".wmv" ".wma" ".m3u" ".flac" ".mid" ".m4a" "aiff" ".mp2" ".aac" ".ogg" ".mov"
        ))

;; Font
(if *on-mac*
    (set-default-font "-apple-Menlo-medium-normal-normal-*-12-*-*-*-m-0-iso10646-1")
)
(if *on-windows*
    (set-default-font "Consolas-10:weight=normal")
)


;;
;; Spacing / Tabs
;;

;; no tabs
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

;; whitespace visualisation
(global-whitespace-mode t)
(setq whitespace-style '(trailing tabs newline tab-mark newline-mark))
(setq whitespace-display-mappings
 '(
    (space-mark 32 [183] [46]) ; normal space
    (space-mark 160 [164] [95])
    (space-mark 2208 [2212] [95])
    (space-mark 2336 [2340] [95])
    (space-mark 3616 [3620] [95])
    (space-mark 3872 [3876] [95])
    (newline-mark 10 [172 10])
    (tab-mark 9 [187 9] [92 9])
))

;; Ensure no mode reverts tab settings
(add-hook 'after-change-major-mode-hook
    '(lambda ()
        (setq-default indent-tabs-mode nil)
        (setq c-basic-offset 4)
        (setq tab-width 4)
))


;;
;; Auto-save and Back-up
;;

(defvar backup-dir (expand-file-name (format "%s/%s/" user-emacs-directory "backup")))
(defvar autosave-dir (expand-file-name (format "%s/%s/" user-emacs-directory "autosave")))

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist (list (cons ".*" backup-dir))
   auto-save-list-file-prefix autosave-dir
   auto-save-file-name-transforms `((".*" ,autosave-dir t))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backupsset window size


;;
;; Windows
;;

;; Shift+{left,up,down,right} window movement
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; toggle 2 window split orientation
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-x 9") 'toggle-frame-split)


;;
;; Key bindings
;;

;; TODO: TIDY UP! Making a big of a mess of things so created key-bindings
;; section even though normally some of these would go in other sections like
;; buffer related settings

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Flip search mode types
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Joins with line above
(global-set-key (kbd "C-j") '(lambda () (interactive) (delete-indentation)))
;; Join with the line below
(global-set-key (kbd "M-j") '(lambda () (interactive) (delete-indentation -1)))

;;previous paragraph
(global-set-key (kbd "M-[") 'backward-paragraph)
;;forward paragraph
(global-set-key (kbd "M-]") 'forward-paragraph)

;; Let me input a hash on a Mac
(fset 'insertPound "#")
(if *on-mac*
    (global-set-key (kbd "M-3") 'insertPound)
)

;; indent / unindent region by 4
;; Note: can reselect region with C-x C-x
;; and can multiply the indentation with C-u 2 > (i.e. to indent by 8)
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

;; TODO: remove unless something goes awry with evil and it's removed
;;(global-set-key ">" 'my-indent-region)
;;(global-set-key "<" 'my-unindent-region)


;; Replace text in whole buffer. The suggested OLD text is either the current region,
;; or the next word (as mark-word would select it). The suggested text for the
;; replacement is the same as the OLD text.
;; After entering the Old / New string you then need to run the replace function.
;; Use search commands to incrementally search forward / backwards or just use !
;; replace all instances.
;; See: https://www.emacswiki.org/emacs/Replace-in-buffer
(defun replace-in-buffer ()
  "Replace text in whole buffer / highlighted text."
  (interactive)
  (save-excursion
    (if (equal mark-active nil) (mark-word))
    (setq curr-word (buffer-substring-no-properties (mark) (point)))
    (setq old-string (read-string "Old string: " curr-word))
    (setq new-string (read-string "New string: " old-string))
    (query-replace old-string new-string nil (point-min) (point-max))
    )
  )

 (global-set-key (kbd "C-z") 'replace-in-buffer)

(require 'hi-lock)
(defun jpt-toggle-mark-word-at-point ()
  "Toggle word highlighting."
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(global-set-key (kbd "s-.") 'jpt-toggle-mark-word-at-point)

;;
;; Buffers
;;

;; Kill all buffers except for the current one
(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-match "^*" (buffer-name)) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

(defun next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (next-buffer) )))

(defun previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-match "^*" (buffer-name))) (< i 50))
      (setq i (1+ i)) (previous-buffer) )))

;; Note: change key bindings
(global-set-key (kbd "<f5>") 'find-file) ; Open file or dir
(global-set-key (kbd "<f6>") 'kill-this-buffer) ; Close file

(global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
(global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
(global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown


;;
;; ediff
;;

;; Note: on windows will need to add diff from cygwin, msys or gnuutils from
;; http://gnuwin32.sourceforge.net/ Currently went with gnuutils and if I do
;; that again I need to also get libiconv2 and libintl3 from there.

;; do not put ediff control panel in its own window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Usage: emacs -diff file1 file2
(defun command-line-diff (switch)
  (let ((file1 (pop command-line-args-left))
        (file2 (pop command-line-args-left)))
    (ediff file1 file2)))

(add-to-list 'command-switch-alist '("diff" . command-line-diff))

;; put things back to the way they were after exiting ediff
(add-hook 'ediff-load-hook
         (lambda ()

           (add-hook 'ediff-before-setup-hook
                     (lambda ()
                       (setq ediff-saved-window-configuration (current-window-configuration))))

           (let ((restore-window-configuration
                  (lambda ()
                    (set-window-configuration ediff-saved-window-configuration))))
             (add-hook 'ediff-quit-hook restore-window-configuration 'append)
             (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))


;;
;; Org Mode
;;

;; C-c C-t followed by sequence key
;; or simply use Shift left/right to cycle list
(setq org-todo-keywords
           '((sequence "TODO(t)" "|" "DONE(d)")
             (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
             (sequence "|" "CANCELED(c)")))

;; org mode to moinmoin
(eval-after-load "org" '(require 'ox-moinmoin nil t))

(org-babel-do-load-languages 'org-babel-load-languages
    '(
      (shell  . t)
      (python . t)
      (perl   . t)
      (css    . t)
    )
)

;;
;; Required Packages
;;
(require 'ido)
(require 'nyan-mode)
(require 'powerline)
(require 'rfringe)
(require 'sr-speedbar)
(require 'transpose-frame)
(require 'web-mode)
(require 'rainbow-mode)
(require 'rainbow-delimiters)
(require 'htmlize)
(require 'java-color-mode)
(require 'yasnippet)
(require 'js2-mode)
(require 'flycheck)


;;
;; Markdown Mode
;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;
;; Nyan-Mode
;;
(setq nyan-bar-length 16)


;;
;; Powerline
;;

;; Keep these as I may want to revert when switching to
;; one of the previous themes I've used
;;(set-face-foreground 'powerline-active1 "white")
;;(set-face-foreground 'powerline-active2 "black")
(setq powerline-default-separator 'bar)

;; config time / date display
;;(setq display-time-format "%a %d %b %Y %H:%M")
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq display-time-load-average 'nil)
(setq display-time-string-forms
    '((propertize (concat " " dayname " " day " " monthname " " year " " 24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
        )))
(display-time)
;; by defualt date/time is added to gsm which means it's auto appended
;; after buffer name - I don't want that.
(setq global-mode-string nil)

(defpowerline display-time display-time-string)

;; borrowed from https://github.com/filinep/.emacs.d/blob/master/powerline_tweak.el
;; TODO: tweak more to my liking

(defun my-powerline-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
    '("%e"
      (:eval
       (let* ((active (powerline-selected-window-active))
              (mode-line (if active 'mode-line 'mode-line-inactive))
              (face1 (if active 'mode-line 'mode-line-inactive))
              (face2 (if active 'mode-line 'mode-line-inactive))
              (separator-left (intern (format "powerline-%s-%s"
                                              powerline-default-separator
                                              (car powerline-default-separator-dir))))

              (separator-right (intern (format "powerline-%s-%s"
                  powerline-default-separator
                  (cdr powerline-default-separator-dir))))

              (lhs (list (powerline-raw "%*" face2 'l)
                 (powerline-buffer-size face2 'l)
                 (powerline-raw mode-line-mule-info face2 'l)
                 (powerline-raw (format "%s" default-directory) face2)
                 ;; (powerline-buffer-id face2 'l)
                 (when (and (boundp 'which-func-mode) which-func-mode)
                   (powerline-raw which-func-format face2 'l))
                 (powerline-raw " " face2)
                 (funcall separator-left face2 face1)
                 (when (boundp 'erc-modified-channels-object)
                   (powerline-raw erc-modified-channels-object face1 'l))
                 (powerline-major-mode face1 'l)
                 (powerline-process face1)
                 (powerline-minor-modes face1 'l)
                 (powerline-narrow face1 'l)
                 (powerline-raw " " face1)
                 (funcall separator-left face1 face2)
                 (powerline-vc face2 'r)))

              (rhs (list (powerline-raw global-mode-string face2 'r)
                 (funcall separator-right face2 face1)
                 (display-time face1 'l)
                 (powerline-raw "%3l" face1 'l)
                 (powerline-raw ":" face1 'l)
                 (powerline-raw "%2c" face1 'l)
                 (powerline-raw "%7p" face1 'l)
                 (powerline-raw (nyan-create) face1)
            )))
      (concat (powerline-render lhs)
        (powerline-fill face2 (powerline-width rhs))
        (powerline-render rhs)))))))

(my-powerline-theme)


;;
;; Flycheck
;;
(setq flycheck-check-syntax-automatically '(mode-enabled save new-line))
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-indication-mode 'right-fringe)
(global-set-key (kbd "<f10>") 'flycheck-list-errors)

;;
;; Flyspell
;;

;; IMPORTANT: ensure aspell and dictionaries of choice are installed
;; and on windows add aspell bin to path
;; Win32: http://aspell.net/win32/
;;
;; Mac - build and install aspell and dicts
;; Mac also needs the full path - at least if compiled form source hence the cond statement


;; easy spell check
(global-set-key (kbd "<f8>") 'ispell-word)
(global-set-key (kbd "C-S-<f8>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f8>") 'flyspell-buffer)
(global-set-key (kbd "C-<f8>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word"
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word)
)
(global-set-key (kbd "M-<f8>") 'flyspell-check-next-highlighted-word)


;;
;; Company Mode
;;
(require 'company)
(require 'company-jedi)


(require 'color)

(let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
     `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))


(defun my/python-mode-hook ()
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)


;;
;; Speedbar
;;

;; Commands
;; C-x C-d     Toggle open/close Speedbar window
;; Enter       Go to the directory / Open the file
;; U           Move the entire speedbar up one directory
;; n           Move cursor to the next item
;; p           Move cursor to the previous item
;; M-n         Move cursor to the next item in restricted fashion
;; M-p         Move cursor to the previous item in restricted fashion
;; SPC         Expand/Collapse the current
;; [           Expand all descendants
;; g           Refresh


(setq speedbar-frame-parameters
    '((minibuffer)
    (width . 44)
    (border-width . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (unsplittable . t)
    (left-fringe . 0)))


(setq speedbar-show-unknown-files t)
(setq speedbar-smart-directory-expand-flag t)
(setq speedbar-verbosity-level 0)

(setq sr-speedbar-width 44)
(setq sr-speedbar-width-x 44)
(setq sr-speedbar-width-console 44)
(setq sr-speedbar-max-width 70)
(setq speedbar-use-images nil)
(setq sr-speedbar-right-side nil)

(global-set-key (kbd "C-x C-d") 'sr-speedbar-toggle)


;;
;; Helm
;;

(require 'helm)
(require 'helm-config)
(require 'helm-eshell)
(require 'helm-files)
(require 'helm-grep)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
(global-set-key (kbd "C-c i") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c o") 'helm-occur)

(setq
    helm-google-suggest-use-curl-p t
    helm-scroll-amount 4             ; scroll 4 lines other window using M-<next>/M-<prior>
    helm-quick-update t              ; do not display invisible candidates
    helm-idle-delay 0.01             ; be idle for this many seconds, before updating in delayed sources.
    helm-input-idle-delay 0.01       ; be idle for this many seconds, before updating candidate buffer
    helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

    ;; you can customize helm-do-grep to execute ack-grep
    ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
    ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
    ;;
    helm-split-window-default-side 'other       ;; open helm buffer in another window
    helm-split-window-in-side-p t               ;; open helm buffer inside current window, not occupy whole other window
    helm-buffers-favorite-modes (append helm-buffers-favorite-modes
                                     '(picture-mode artist-mode))
    helm-candidate-number-limit 200             ; limit the number of displayed canidates
    helm-M-x-requires-pattern 0                 ; show all candidates when set to 0
    ;; do not show these files in helm buffer
    helm-boring-file-regexp-list
        '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\.idea$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$" "\\.pyc$" "\\.pyo$")

    helm-ff-file-name-history-use-recentf t
    helm-ff-skip-boring-files t
    helm-move-to-line-cycle-in-source t     ; move to end or beginning of source
                                            ; when reaching top or bottom of source.
    ido-use-virtual-buffers t               ; Needed in helm-buffers-list
    helm-buffers-fuzzy-matching t           ; fuzzy matching buffer names when non--nil
                                            ; useful in helm-mini that lists buffers
)

(define-key helm-map (kbd "C-x 2") 'helm-select-2nd-action)
(define-key helm-map (kbd "C-x 3") 'helm-select-3rd-action)
(define-key helm-map (kbd "C-x 4") 'helm-select-4rd-action)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h m") 'helm-man-woman)
(global-set-key (kbd "C-c h g") 'helm-do-grep)
(global-set-key (kbd "C-c h p") 'helm-projectile-grep)
(global-set-key (kbd "C-c h f") 'helm-find)
(global-set-key (kbd "C-c h l") 'helm-locate)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h r") 'helm-resume)
(global-set-key (kbd "C-h C-f") 'helm-apropos)

;; use helm to list eshell history
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)))

;; Save current position to mark ring
(add-hook 'helm-goto-line-before-hook 'helm-save-current-pos-to-mark-ring)

;; Add helm-do-grep back
(defun helm-do-grep (&optional arg)
  (interactive "P")
  (helm-do-grep-1 (list default-directory) arg))

(helm-mode 1)


;;
;; Rainbow Mode
;;

;; set modes
(defun all-css-modes() (css-mode) (rainbow-mode))
;; load modes
(add-to-list 'auto-mode-alist '("\\.css$" . all-css-modes))


;;
;; Rainbow Delimiters Mode
;;

;; Enabled for all programming related modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;
;; Web-Mode
;;
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(defun web-mode-hook ()
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-style-padding 0)
    (setq web-mode-script-padding 0)
    (setq web-mode-block-padding 0)
    (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
)
(add-hook 'web-mode-hook 'web-mode-hook)


;;
;; Rust Mode
;;
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))


;;
;; Perl
;;

;; Use CPerl mode
(defalias 'perl-mode 'cperl-mode)
;; do not want to match trailing whitespace with underline
(setq cperl-invalid-face nil)
(setq cperl-indent-level 4
      cperl-close-paren-offset -4
      cperl-continued-statement-offset 4
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;; shorter perldoc command
(add-hook 'cperl-mode-hook
    (lambda ()
        (local-set-key (kbd "C-h f") 'cperl-perldoc)))


;;
;; Python
;;

(add-hook 'python-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'python-pylint)
(setq-default flycheck-flake8-maximum-line-length 120)


;;
;; Ruby
;;

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.builder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-to-list 'flycheck-disabled-checkers 'ruby-pylint)
(add-to-list 'flycheck-disabled-checkers 'ruby-jruby)
(add-to-list 'flycheck-disabled-checkers 'ruby-rubocop)


;;
;; Java Mode
;;

(add-hook 'java-mode-hook 'java-color-mode)


;;
;; Yasnippets
;;

;; seems to do the indenting as I want. Will use it a bit
;; and see what happens
(setq yas-indent-line 'fixed)

(setq yas-snippet-dirs
      '((format "%s/%s" user-emacs-directory "snippets")
        ))
;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

;; programming only
(add-hook 'prog-mode-hook
    '(lambda ()
        (yas-minor-mode)))


;;
;; JS2 Mode
;;

(add-hook 'js-mode-hook 'js2-minor-mode)
;;(add-hook 'js-mode-hook 'flycheck-mode)
;;(add-to-list 'flycheck-disabled-checkers 'javascript-eslint)
;;(add-to-list 'flycheck-disabled-checkers 'javascript-gjslint)
;;(add-to-list 'flycheck-disabled-checkers 'javascript-jscs)
;;(add-to-list 'flycheck-disabled-checkers 'javascript-standard)


;;
;; Yaml Mode
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
  '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))


;;
;; Undo Tree
;;
(require 'undo-tree)
(global-undo-tree-mode 1)

(setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)

(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist `(("." . ,(expand-file-name (format "%s/%s/" user-emacs-directory "undo")))))

(defadvice undo-tree-make-history-save-file-name
     (after undo-tree activate)
     (setq ad-return-value (concat ad-return-value ".zip")))


;;
;; Hydra
;;

;; Functions that resize windows
(defun hydra-move-splitter-left (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))


(require 'hydra)

; amaranth (non-hydra key presses don't cancel mode)
(global-set-key
 (kbd "C-M-y")
 (defhydra hydra-move-window-splitter (:color amaranth)
   "
^Move to Window^    ^Resize Window^    ^Misc^
^^^^^^^^---------------------------------------------------------
_h_: left           _H_: left          _>_: split horizontal
_j_: down           _J_: down          _V_: split vertical
_k_: up             _K_: up            _t_: toggle split
_l_: right          _L_: right         _o_: delete other windows
^^
_q_: quit
^^
"
   ("h" windmove-left nil)
   ("j" windmove-down nil)
   ("k" windmove-up nil)
   ("l" windmove-right nil)

   ("H" hydra-move-splitter-left nil)
   ("J" hydra-move-splitter-down nil)
   ("K" hydra-move-splitter-up nil)
   ("L" hydra-move-splitter-right nil)

   (">" split-window-horizontally nil)
   ("V" split-window-vertically nil)

   ("t" transpose-frame nil)
   ("o" delete-other-windows nil :color blue)

   ("q" nil nil)
 ))

; Grow / Shrink text
(global-set-key
 (kbd "C-M-z")
 (defhydra hydra-zoom (:color amaranth)
  "zoom"
  ; grow
  ("g" text-scale-increase "in")
  ; shrink
  ("s" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset" :color blue)
  ("q" nil "quit")
))

;; Undo tree common functionality
(defhydra hydra-undo-tree (:color yellow)
  "
  _u_: undo  _r_: redo _s_: save _l_: load
  "
  ("u"   undo-tree-undo)
  ("r"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("v"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))

(global-set-key (kbd "C-M-u") 'hydra-undo-tree/undo-tree-undo)


;;
;; Jenkins
;;
;;(require 'jenkins)
;;(setq jenkins-api-token "<api token can be found on user's configure page>")
;;(setq jenkins-hostname "<jenkins url>")
;;(setq jenkins-username "<your user name>")
;; Uncomment if using a view
;;(setq jenkins-viewname "<viewname>")


;;
;; Magit
;;
(add-to-list 'load-path (format "%s/%s/%s/%s/" user-emacs-directory "packages" "magit" "lisp"))
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               (format "%s/%s/%s/%s/" user-emacs-directory "packages" "magit" "Documentation")))


;;
;; Git Gutter
;;
(require 'git-gutter-fringe+)
;; git-gutter will be enabled via the fringe addition
(global-git-gutter+-mode)
(setq git-gutter-fr+-side 'right-fringe)
(global-set-key (kbd "C-x g") 'git-gutter+-mode) ; Turn on/off in the current buffer
(global-set-key (kbd "C-x G") 'global-git-gutter+-mode) ; Turn on/off globally
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))


;;
;; Projectile
;;
(require 'projectile)
(require 'helm-projectile)
(setq projectile-enable-caching t)
(projectile-global-mode)

;; Ignore list for all projects
(setq projectile-globally-ignored-directories
      (append '(
        ".git"
        ".svn"
        ".idea"
        "out"
        "repl"
        "target"
        "venv"
        )
          projectile-globally-ignored-directories))
(setq projectile-globally-ignored-files
      (append '(
        ".DS_Store"
        "*.gz"
        "*.pyc"
        "*.jar"
        "*.tar.gz"
        "*.tgz"
        "*.zip"
        "*.png"
        ".gif"
        ".bmp"
        "*.jpeg"
        "*.jpg"
        )
          projectile-globally-ignored-files))
(projectile-global-mode)


;; delete trailing whitespace in programming modes
;; Maybe consider:
;; delete trailing whitespace before save
;;(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'prog-mode-hook
    '(lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))


;;
;; Docker
;;
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
;; Use "docker-machine env box" command to find out your environment variables
;;(setenv "DOCKER_TLS_VERIFY" "1")
;;(setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
;;(setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
;;(setenv "DOCKER_MACHINE_NAME" "box")

;;
;; Evil Mode
;;
;; TODO: Buggy - need to either revert or investigate where it goes wrong
(require 'evil)
(evil-mode 1)

(provide 'init)
;;; init.el ends here
