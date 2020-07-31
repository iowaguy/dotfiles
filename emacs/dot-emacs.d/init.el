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

(org-babel-load-file "~/.emacs.d/configuration.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (osx-dictionary ox-twbs eshell-git-prompt comment-dwim-2 yaml-mode ws-butler which-key wgrep web-mode use-package tuareg solarized-theme smex slim-mode scss-mode scala-mode sbt-mode rust-mode rspec-mode rainbow-mode rainbow-delimiters racket-mode python-mode py-autopep8 projectile powerthesaurus pinboard paredit orgalist org-plus-contrib org-bullets multi-term moody minions merlin instapaper htmlize helpful haskell-mode haml-mode graphviz-dot-mode go-errcheck gnuplot git-timemachine forge flycheck-package flx evil-magit engine-mode emms elpy elfeed-org eglot dumb-jump dired-open dired-hide-dotfiles diff-hl deft counsel company-restclient company-jedi company-go company-coq coffee-mode chruby avy auto-compile ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t nil))))
