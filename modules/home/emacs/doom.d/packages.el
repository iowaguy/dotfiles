;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;; ;; I like tree-based undo management. I only rarely need it, but when I do, oh boy.
(package! undo-tree)

;; ;; I use =eglot= as my LSP client.
(package! eglot)

;; ;; Make thin vertical line at 80 characters.
(package! fill-column-indicator)

;; Don't compile the current SCSS file every time I save.
(package! scss-mode)

;; I like to use =paredit= in Lisp modes to balance parentheses (and more!).
(package! paredit)

;; =rainbow-delimiters= is convenient for coloring matching parentheses.
(package! rainbow-delimiters)

;; Use =direnv= with emacs files. When I use a file, it will use the =direnv=
;; environment of that file.
(package! direnv)
  ;; :config
  ;; (direnv-mode))

;; Set a theme for eshell which has the git status in the prompt.
(package! eshell-git-prompt)

(package! helm)

;; This has a bunch of utilities for citing documents within =org-mode=. To cite
;; something, type =C-c ]=.
;;(package! org-ref)

;; ;; Export to bootstrap html
;; (package! ox-twbs)

;; ;; Export to a cool slideshow thing
;; (package! ox-reveal)


;;;;;;;;;;;;;;; Org-roam stuff ;;;;;;;;;;;;;;;
;; More meta-data for my annotated bibliography.
;;(package! org-roam-bibtex
;;  :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; A nice visualizer for my =org-roam= graph. Needs latest version
;; of org-roam, so I need to unpin.
;;(unpin! org-roam)
;;(package! org-roam-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use the [[https://elpa.gnu.org/packages/orgalist.html][=orgalist=]] package for more convenient list manipulation.
(package! orgalist)

(package! palimpsest)

;; Hide dotfiles by default, but toggle their visibility with =.=.
(package! dired-hide-dotfiles)

;; Open media with the appropriate programs.
(package! dired-open)

;; Completions
(package! counsel)
(package! flx)

;; Maintain completion history
(package! smex)

;; Enable [[https://github.com/hrs/engine-mode][engine-mode]] and define a few useful engines.
(package! engine-mode)

(package! dumb-jump)

(package! let-alist)

(package! flycheck)

(package! magit)

(package! projectile)

;; Treating terms in CamelCase symbols as separate words makes editing a little
;; easier for me, so I like to use =subword-mode= everywhere.
(package! subword)

;; If I'm writing in Emacs lisp I'd like to use =eldoc-mode= to display
;; documentation.
;;(package! eldoc)

;; Better commenting
(package! comment-dwim-2)

(package! buffer-move :recipe (:host github :repo "lukhas/buffer-move"))

(package! vagrant-tramp)

(package! rg)

(package! chronos)

;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(package! helpful)

(package! promela-mode :recipe (:host github
                                :repo "rudi/promela-mode"
                                :files (:defaults "promela-mode.el")))

;;(package! org :pin "806abc5a2bbcb5f884467a0145547221ba09eb59")
