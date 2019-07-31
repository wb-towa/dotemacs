
;;; init-packages -- Packaging related things

(require 'package)
(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("melpa"        . 5)
        ("gnu"          . 0))
)
;; package install location
(setq package-user-dir
      (format "%s/%s" user-emacs-directory "elpa"))

;; TODO: Work on a fix for helm-make (PR perhaps)
;; it's fixed but if it gets updated and breaks then
;; remove the elc files and add:
;;(eval-when-compile
;;  (require 'helm-source nil t))
(defconst wb-packages
    '(ample-theme
    async
    ctable
    company
    company-jedi
    dash
    darktooth-theme
    deferred
    docker
    epc
    flatland-theme
    fringe-helper
    flycheck
    flycheck-swift
    git-gitter
    git-gutter+
    git-gutter-fringe
    git-gutter-fringe+
    go-mode
    gruber-darker-theme
    helm
    helm-make
    helm-projectile
    hydra
    htmlize
    inkpot-theme
    jedi
    jenkins
    js2-mode
    magit
    markdown-mode
    moe-theme
    nyan-mode
    oldlace-theme
    popup
    powerline
    projectile
    python-environment
    rainbow-mode
    rainbow-delimiters
    rust-mode
    s
    soft-stone-theme
    solarized-theme
    sr-speedbar
    subatomic-theme
    sunny-day-theme
    swift-mode
    tango-plus-theme
    tangotango-theme
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
