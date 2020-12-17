;; Configure package.el to include MELPA.
(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; This line is needed to follow the symlink to configuration-private.org which
;; is in a git controlled repo
(setq vc-follow-symlinks nil)

;; Load actual configurations
(org-babel-load-file "~/.emacs.d/configuration-private.org")
(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/workspace/dotfiles/emacs/dot-emacs.d/configuration.org" "~/Dropbox/techno-happiness/research.org" "~/Dropbox/org/thebigone.org" "~/Dropbox/org/schedule.org"))
 '(package-selected-packages
   '(org-roam-bibtex emacs-zotero-bib-fetch zotelo org-roam zetteldeft helm org-ref yaml-mode ws-butler which-key wgrep web-mode use-package undo-tree solarized-theme smex scss-mode rust-mode rainbow-mode rainbow-delimiters racket-mode python-mode py-autopep8 projectile powerthesaurus pdf-tools paredit palimpsest ox-twbs ox-reveal osx-dictionary orgalist org-plus-contrib org-gcal org-bullets ob-ipython ob-async multi-term moody minions jupyter htmlize helpful haskell-mode graphviz-dot-mode go-errcheck gnuplot git-timemachine forge flycheck-package flx exec-path-from-shell eshell-git-prompt engine-mode elpy eglot dumb-jump dired-open dired-hide-dotfiles diff-hl deft counsel company-jedi company-go comment-dwim-2 coffee-mode avy auto-compile ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
