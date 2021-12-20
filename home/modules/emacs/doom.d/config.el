(defun blw/set-macos-vars ()
  (setq blw/open-firefox "open -a Firefox")
  (setq blw/screenshot "screencapture -i %s")

  ;; Bind =alt= to =super=
  (setq ns-alternate-modifier (quote super))

  ;; Bind =command= to =meta=
  (setq ns-command-modifier (quote meta))

  ;; Bind =fn= to =control=
  (setq ns-function-modifier (quote control)))

(defun blw/set-linux-vars ()
  (setq blw/open-firefox "firefox --newtab --url")
  (setq blw/screenshot "scrot --select %s"))

(defun blw/set-system-specific-vars ()
  (cond
   ((string-equal system-type "darwin") ;  macOS
    (blw/set-macos-vars))
   ((string-equal system-type "gnu/linux")
    (blw/set-linux-vars))))

(blw/set-system-specific-vars)

(setq user-full-name "Ben Weintraub"
      user-mail-address "ben@weintraub.xyz"
      calendar-latitude 42.35
      calendar-longitude -71.06
      calendar-location-name "Boston, MA"
      blw/dark-theme 'doom-one
      blw/light-theme 'doom-one-light)

(defun hrs/rename-file (new-name)
  (interactive "FNew name: ")
  (let ((filename (buffer-file-name)))
    (if filename
        (progn
          (when (buffer-modified-p)
            (save-buffer))
          (rename-file filename new-name t)
          (kill-buffer (current-buffer))
          (find-file new-name)
          (message "Renamed '%s' -> '%s'" filename new-name))
      (message "Buffer '%s' isn't backed by a file!" (buffer-name)))))

(defun hrs/generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random
       name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun hrs/append-to-path (path)
  "Add a path both to the $PATH variable and to Emacs exec-path."
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (add-to-list 'exec-path path))

(defun hrs/notify-send (title message)
  "Display a desktop notification by shelling out to `notify-send'."
  (call-process-shell-command
   (format "notify-send -t 2000 \"%s\" \"%s\"" title message)))

(defun blw/apply-dark-theme ()
  "Apply the dark theme."
  (interactive)
  (load-theme blw/dark-theme t))

(defun blw/apply-light-theme ()
  "Apply the light theme."
  (interactive)
  (load-theme blw/light-theme t))

(use-package doom-themes
  :config
  (blw/apply-dark-theme))

(setq ring-bell-function 'ignore)

(setq scroll-conservatively 100)

(setq hrs/default-font "Inconsolata")
(setq hrs/default-font-size 24)
(setq hrs/current-font-size hrs/default-font-size)

(setq hrs/font-change-increment 1.1)

(defun hrs/font-code ()
  "Return a string representing the current font (like \"Inconsolata-14\")."
  (concat hrs/default-font "-" (number-to-string hrs/current-font-size)))

(defun hrs/set-font-size ()
  "Set the font to `hrs/default-font' at `hrs/current-font-size'.
Set that for the current frame, and also make it the default for
other, future frames."
  (let ((font-code (hrs/font-code)))
    (if (assoc 'font default-frame-alist)
        (setcdr (assoc 'font default-frame-alist) font-code)
      (add-to-list 'default-frame-alist (cons 'font font-code)))
    (set-frame-font font-code)))

(defun hrs/reset-font-size ()
  "Change font size back to `hrs/default-font-size'."
  (interactive)
  (setq hrs/current-font-size hrs/default-font-size)
  (hrs/set-font-size))

(defun hrs/increase-font-size ()
  "Increase current font size by a factor of `hrs/font-change-increment'."
  (interactive)
  (setq hrs/current-font-size
        (ceiling (* hrs/current-font-size hrs/font-change-increment)))
  (hrs/set-font-size))

(defun hrs/decrease-font-size ()
  "Decrease current font size by a factor of `hrs/font-change-increment', down to a minimum size of 1."
  (interactive)
  (setq hrs/current-font-size
        (max 1
             (floor (/ hrs/current-font-size hrs/font-change-increment))))
  (hrs/set-font-size))

(define-key global-map (kbd "C-)") 'hrs/reset-font-size)
(define-key global-map (kbd "C-+") 'hrs/increase-font-size)
(define-key global-map (kbd "C-=") 'hrs/increase-font-size)
(define-key global-map (kbd "C-_") 'hrs/decrease-font-size)
(define-key global-map (kbd "C--") 'hrs/decrease-font-size)

(hrs/reset-font-size)

(use-package! diff-hl
  :defer t
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package! buffer-move
  :defer t)

(global-set-key (kbd "<C-S-up>")     #'buf-move-up)
(global-set-key (kbd "<C-S-down>")   #'buf-move-down)
(global-set-key (kbd "<C-S-left>")   #'buf-move-left)
(global-set-key (kbd "<C-S-right>")  #'buf-move-right)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(set-frame-parameter nil 'fullscreen 'maximized)

(use-package! magit
  :defer t
  :bind
  ("C-c m" . magit-status)

  :config
  (use-package! with-editor)

  (setq magit-push-always-verify nil
        git-commit-summary-max-length 50))

(use-package! projectile
  :defer t
  :bind
  ("C-c v" . projectile-ag)

  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (setq projectile-completion-system 'ivy
        projectile-switch-project-action 'projectile-dired
        projectile-require-project-root nil))

(setq-default tab-width 2)

(use-package! subword
  :defer t
  :config (global-subword-mode 1))

(setq compilation-scroll-output t)

(use-package! rg
  :defer t)

(use-package! eldoc
  :defer t
  :config
  (add-hook! 'emacs-lisp-mode-hook 'eldoc-mode))

(eshell-git-prompt-use-theme 'robbyrussell)

(setq eshell-list-files-after-cd t)

(global-set-key [f1] 'eshell)

(use-package! exec-path-from-shell
  :defer t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package! org-tempo
  :after org)

(setq initial-major-mode 'org-mode)

(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

(use-package! notmuch
  :defer t
  :config
  (setq message-default-mail-headers "Cc: \nBcc: \n"
        +notmuch-sync-backend 'mbsync
        notmuch-hello-sections `(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        ;; The following three make sure that what email a message was addressed
        ;; to will be used as the from address in my reply.
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header
        mail-user-agent 'message-user-agent
        sendmail-program "msmtp"

        notmuch-saved-searches
        '((:name "slimbox"
           :query "tag:unread AND NOT tag:scholarly-reading AND NOT tag:calnewport AND NOT tag:newyorker AND NOT tag:lightning-dev AND NOT tag:nu-unread"
           :key "i"
           :sort-order newest-first
           :search-type 'tree)
          (:name "inbox" :query "tag:inbox" :sort-order newest-first :search-type 'tree)
          (:name "unread" :query "tag:unread" :key "u" :sort-order newest-first :search-type 'tree)
          (:name "nu-unread" :query "to:weintraub.b@northeastern.edu AND tag:unread", :key "nu" :sort-order newest-first :search-type 'tree)
          (:name "calnewport" :query "tag:calnewport AND tag:unread" :key "cn" :sort-order newest-first :search-type 'tree)
          (:name "lightning-dev" :query "tag:lightning-dev AND tag:unread" :sort-order newest-first :key "l" :search-type 'tree)
          (:name "newyorker" :query "tag:newyorker AND tag:unread" :sort-order  newest-first :key "ny" :search-type 'tree)
          (:name "scholarly-reading" :query "tag:scholarly-reading AND tag:unread" :sort-order newest-first :key "s" :search-type 'tree)
          (:name "the-economist" :query "tag:economist AND tag:unread" :sort-order newest-first :key "e" :search-type 'tree)
          (:name "recent" :query "date:1week..today" :sort-order newest-first :key "e" :search-type 'tree)
          (:name "sent"    :query "tag:sent" :key "s")
          (:name "drafts"  :query "tag:draft":key "d"))))

(after! org-mime
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))
(map! :after notmuch
      :map message-mode-map
      :localleader
      :desc "Edit in Org" "o" #'org-mime-edit-mail-in-org-mode)

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(use-package! org
  :defer t
  :config
  (defalias '+org--restart-mode-h #'ignore))

(after! org
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
               '("jp" . "src jupyter-python :async yes :exports code :results code")))

(setq org-pretty-entities t)

(defun blw/full-file-path (directory filename)
  "Return the absolute path of a file, given its filename and the directory it's in."
  (concat (file-name-as-directory directory) filename))

(defun blw/org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (blw/full-file-path org-directory filename))

(defun blw/persistent-file-path (filename)
  "Return the absolute address of a file in whatever persistent storage I'm using, given its relative name."
  (blw/full-file-path blw/org-persistent-directory filename))

(defun blw/zettelkasten-path (filename)
  "Return the absolute path of a file in my zettelkasten."
  (concat (blw/persistent-file-path "zettelkasten/") filename))

(setq! blw/org-persistent-directory "~/workspace/"
       org-directory (blw/persistent-file-path "org")
       org-index-file (blw/org-file-path "todos.org")
       org-gcal-file (blw/org-file-path "schedule.org")
       blw/org-cs-reading-file (blw/org-file-path "reading.org")
       blw/org-writing-reading-file (blw/org-file-path "reading_writing.org")
       blw/org-fun-projects-file (blw/org-file-path "fun-projects.org")
       blw/org-zettel-inbox-file (blw/org-file-path "zettel-inbox.org")
       blw/zettelkasten (blw/zettelkasten-path ""))

(setq org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
                                 (todo . " %i ")
                                 (tags . " %i ")
                                 (search . " %i ")))

(after! org
        (setq org-agenda-restore-windows-after-quit t

              ;; Open org-agenda in other side of split, making the split if
              ;; necessary.
              org-agenda-window-setup 'other-window
              org-agenda-skip-scheduled-if-deadline-is-shown t
              org-agenda-skip-deadline-if-done t
              org-agenda-span 'week
              org-agenda-start-day nil

              ;; Add more S-expressions =org-file-path= to the list to have
              ;; them parsed for my org-agenda.
              org-agenda-files (list org-index-file
                                     blw/org-cs-reading-file
                                     blw/org-writing-reading-file)
              ;; Record the time that a todo was completed.
              org-log-done 'time))

(defun hrs/org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun hrs/org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("p" "Personal agenda"
         ((agenda "")
          (todo "TODO|SHALLOW|DEEP"
                ((org-agenda-skip-function '(or (org-agenda-skip-subtree-if 'scheduled)
                                                (org-agenda-skip-subtree-if 'deadline)))
                 (org-agenda-overriding-header "Other tasks:")))
          (todo "INPROGRESS"
                ((org-agenda-overriding-header "In progress:")))
          (todo "READ|SKIM|NOTES"
                ((org-agenda-overriding-header "CS Reading:")))
          (todo "READ_W|SKIM_W|NOTES_W"
                ((org-agenda-overriding-header "Writing Reading:")))
          (todo "VET"
                ((org-agenda-overriding-header "CS Vetting:")))
          (todo "VET_W"
                ((org-agenda-overriding-header "Writing Vetting:")))))))

(defun hrs/dashboard ()
  (interactive)
  (org-agenda nil "p"))

(global-set-key (kbd "C-c d") 'hrs/dashboard)

(after! org-roam-server
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(defun blw/org-roam-graph-firefox (graph)
  (org-roam-server-mode t)
  (shell-command (concat blw/open-firefox " http://127.0.0.1:8080")))

(defun blw/get-string-from-file (filePath)
  "Return filePath's file content."
  (concat (with-temp-buffer
            (insert-file-contents filePath)
            (buffer-string))))

(after! org-roam
  ;; This must be a string because the variable gets used before the
  ;; convenience functions in this file get called.
  (setq org-roam-directory "~/workspace/zettelkasten"
        org-roam-db-update-method 'immediate
        org-roam-graph-executable "dot"
        org-roam-graph-shorten-titles 'wrap
        org-roam-graph-max-title-length 50
        org-roam-graph-viewer 'blw/org-roam-graph-firefox
        org-roam-graph-extra-config '(("overlap" . "no"))
        org-roam-graph-exclude-matcher '("index" "daily")
        org-roam-graph-executable "neato"
        org-roam-dailies-directory "daily/"
        +org-roam-open-buffer-on-find-file nil
        emacsql-sqlite3-executable (executable-find "sqlite3")
        org-roam-completion-everywhere nil
        org-roam-dailies-capture-templates
         '(("x" "default" entry
           #'org-roam-capture--get-point
           "* %?"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+title: %<%Y-%m-%d>\n\n")))

  (map! :map org-mode-map
        (:prefix ("C-c n" . "org-roam")
        :desc "Insert"                "i" #'org-roam-node-insert
        :desc "Unlink"                "u" #'org-link-at-point-unlink)))

(map! "C-c n d" :desc "Today" #'org-journal-new-entry)
(map! "C-c n a" :desc "Any day" #'org-journal-new-scheduled-entry)

(org-roam-bibtex-mode)

(require 'subr-x) ;; for `when-let'

(defun org-link-at-point-unlink ()
  "Replace link at point with description."
  (interactive)
  (when-let ((el (org-element-context))
         (b (and (eq (org-element-type el) 'link)
             (org-element-property :contents-begin el)))
         (e (org-element-property :contents-end el))
         (contents (buffer-substring-no-properties b e))
         (b (org-element-property :begin el))
         (e (org-element-property :end el)))
    (delete-region b e)
    (insert contents)))

(setq! org-journal-file-type 'weekly
       org-journal-file-header 'blw/org-journal-file-header-func
       org-journal-time-format "plan")

(defun blw/org-journal-file-header-func ()
  "Custom function to create journal header."
  (concat
    (pcase org-journal-file-type
      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything")
      (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: showeverything")
      (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
      (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded"))))

(setq org-download-image-dir "screenshots/")
(setq org-download-image-org-width 500)

(use-package! org-ref
  :after org
  :config
  ;; see org-ref for use of these variables
  (setq org-ref-default-bibliography (blw/zettelkasten-path "references.bib")
        reftex-default-bibliography (blw/zettelkasten-path "references.bib")
        bibtex-completion-bibliography (blw/zettelkasten-path "references.bib")
        bibtex-completion-library-path (blw/zettelkasten-path "bibtex.pdfs")
        bibtex-completion-notes-path blw/zettelkasten))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))

(setq doc-view-resolution 192)

(after! org
        (setq org-capture-templates nil)
        (setq org-capture-templates
              '(("r" "CS Reading material"
                 entry
                 (file blw/org-cs-reading-file)
                 "* %?\n")
                ("w" "Writing Reading material"
                 entry
                 (file blw/org-writing-reading-file)
                 "* %?\n")
                ("c" "CV/Website updates"
                 entry
                 (file "~/workspace/org/cv-website.org")
                 "* %?\n")
                ("z" "Zettel inbox"
                 entry
                 (file blw/org-zettel-inbox-file)
                 "* %?\n")
                ("t" "TODO inbox"
                 entry
                 (file org-index-file)
                 "* TODO %?\n")
                ("f" "Fun projects"
                 entry
                 (file blw/org-fun-projects-file)
                 "* %?\n"))))

(defun blw/zettel-topics ()
  (interactive)
  (find-file blw/org-zettel-inbox-file))

(defun blw/todos ()
  (interactive)
  (find-file org-index-file))

(defun blw/fun-projects ()
  (interactive)
  (find-file blw/org-fun-projects-file))

(defun blw/cs-reading-list ()
  (interactive)
  (find-file blw/org-cs-reading-file))

(defun blw/writing-reading-list ()
  (interactive)
  (find-file blw/org-writing-reading-file))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package! ox-twbs
  :after org
  :config
  (require 'ox-md)
  (require 'ox-beamer)
  (require 'ox-twbs))

(use-package! htmlize
    :defer t)

(setq org-html-postamble nil)

(setq org-html-htmlize-output-type 'inline-css)
(setq org-twbs-htmlize-output-type 'inline-css)

(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "bibtex %b"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(after! org-tempo
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted))

(setq org-latex-caption-above nil)

(setq TeX-PDF-mode t)

(add-hook! 'LaTeX-mode-hook
          (lambda ()
            (LaTeX-math-mode)))

(add-hook! 'LaTeX-mode-hook 'auto-fill-mode)

(defvar prose-modes
  '(gfm-mode
    git-commit-mode
    markdown-mode
    message-mode
    mu4e-compose-mode
    org-mode
    text-mode))

(defvar prose-mode-hooks
  (mapcar (lambda (mode) (intern (format "%s-hook" mode)))
          prose-modes))

(dolist (hook prose-mode-hooks)
  (add-hook hook 'turn-on-auto-fill))

(add-hook 'git-commit-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'message-mode-hook 'orgtbl-mode)

(use-package! orgalist
  :defer t
  :config
  (add-hook 'git-commit-mode-hook 'orgalist-mode)
  (add-hook 'markdown-mode-hook 'orgalist-mode)
  (add-hook 'message-mode-hook 'orgalist-mode))

(use-package! dired-hide-dotfiles
  :defer t
  :config
  (dired-hide-dotfiles-mode)
  (define-key dired-mode-map "." 'dired-hide-dotfiles-mode))

(setq-default dired-listing-switches "-lhvA")
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

(setq dired-clean-up-buffers-too t)

(setq dired-recursive-copies 'always)

(setq dired-recursive-deletes 'top)

(use-package! async
  :defer t
  :config
  (dired-async-mode 1))

(defun hrs/visit-emacs-config ()
  (interactive)
  (find-file "~/.doom.d/config.org"))

(global-set-key (kbd "C-c e") 'hrs/visit-emacs-config)

(defun hrs/kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'hrs/kill-current-buffer)

(use-package! helpful)

(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

(hrs/append-to-path "/usr/local/bin")

(save-place-mode t)

(setq-default indent-tabs-mode nil)

(use-package! which-key
  :defer t
  :config (which-key-mode))

(use-package! counsel
  :defer t
  :bind
  ("M-x" . 'counsel-M-x)
  ("C-s" . 'swiper)

  :config
  (use-package! flx)
  (use-package! smex)

  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(defun hrs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun hrs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(global-set-key (kbd "C-x 2") 'hrs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'hrs/split-window-right-and-switch)

(use-package! engine-mode
    :defer t)
(require 'engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine google-scholar
  "https://scholar.google.com/scholar?hl=en&q=%s"
  :keybinding "s")

(defengine rfcs
  "http://pretty-rfc.herokuapp.com/search?q=%s")

(engine-mode t)

(use-package! comment-dwim-2
  :defer t
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2))

(use-package! chronos
  :defer t
  :config
  (setq chronos-expiry-functions '(chronos-dunstify)))

(setq evil-escape-unordered-key-sequence t)

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

(remove-hook 'org-mode-hook #'+literate-enable-recompile-h)

(use-package! promela-mode)
(autoload 'promela-mode "promela-mode" "PROMELA mode" nil t)
(setq auto-mode-alist
      (append
        (list (cons "\\.promela$"  'promela-mode)
        (cons "\\.spin$"     'promela-mode)
        (cons "\\.pml$"      'promela-mode)
        ;; (cons "\\.other-extensions$"     'promela-mode)
              )
        auto-mode-alist))
