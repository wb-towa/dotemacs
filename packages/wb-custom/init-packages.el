(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

;; package install location
(setq package-user-dir
      (format "%s/%s" user-emacs-directory "elpa"))

;; TODO: Work on a fix for helm-make (PR perhaps)
;; it's fixed but if it gets updated and breaks then
;; remove the elc files and add:
;;(eval-when-compile
;;  (require 'helm-source nil t))
(defconst wb-packages
    '(async
    ctable
    company
    company-jedi
    dash
    deferred
    epc
    fringe-helper
    flycheck
    git-gitter
    git-gutter+
    git-gutter-fringe
    git-gutter-fringe+
    helm
    helm-make
    helm-projectile
    hydra
    htmlize
    jedi
    jenkins
    js2-mode
    magit
    markdown-mode
    nyan-mode
    popup
    powerline
    projectile
    python-environment
    rainbow-mode
    rainbow-delimiters
    rust-mode
    s
    sr-speedbar
    transpose-frame
    undo-tree
    web-mode
    yaml-mode
    yasnippet)
  "List of packages that I like.")

;; activate packages
(package-initialize)

;; fetch list of available packages
(unless package-archive-contents
  (package-refresh-contents))


;; install required
(dolist (package wb-packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
;;(save-window-excursion
;;  (package-list-packages t)
;;  (package-menu-mark-upgrades)
;;  (condition-case nil
;;      (package-menu-execute t)
;;    (error
;;     (package-menu-execute))))

(provide 'init-packages)