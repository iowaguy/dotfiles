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
(let ((f "~/.emacs.d/configuration-private.org"))
  (if (file-exists-p f)
      (org-babel-load-file "~/.emacs.d/configuration-private.org")))
(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(notmuch use-package yaml-mode ws-butler which-key wgrep web-mode undo-tree solarized-theme smex scss-mode rust-mode rainbow-mode rainbow-delimiters racket-mode python-mode py-autopep8 projectile powerthesaurus paredit palimpsest ox-twbs ox-reveal osx-dictionary orgalist org-roam-server org-roam-bibtex org-ref org-plus-contrib org-gcal org-download org-bullets ob-async nix-mode multi-term moody minions jupyter helpful haskell-mode go-errcheck git-timemachine forge flycheck-package flx fill-column-indicator exec-path-from-shell eshell-git-prompt engine-mode elpy eglot dumb-jump direnv dired-open dired-hide-dotfiles diff-hl deft counsel company-jedi company-go comment-dwim-2 coffee-mode avy auto-compile ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
