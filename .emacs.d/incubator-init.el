 (use-package impatient-mode
  :ensure t
  :commands (impatient-mode)
  :hook (impatient-mode . vh/impatient-mode-func)
  :config
  (defun is-html-buffer ()
    (equal "html" (file-name-extension (or (buffer-file-name (current-buffer)) ""))))

  (defun vh/impatient-mode-func ()
    (if (is-html-buffer)
        (imp-set-user-filter (lambda (content)
                               (let ((data (with-current-buffer content (buffer-string))))
                                 (princ data (current-buffer))))))))

(use-package plantuml-mode
  :bind (:map plantuml-mode-map
              ("C-c C-p b" . plantuml-preview-buffer)
              ("C-c C-p p" . plantuml-preview))
  :mode "\\.puml$"
  :init
  (setq plantuml-exec-mode 'jar)
  :custom ((plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
           (plantuml-default-exec-mode 'jar)
           (org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"))
  :config
  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))

(defun vh/buffer-visible-p (buf)
  (memq buf
        (mapcan (lambda (f)
                  (mapcar (lambda (w) (window-buffer w))
                          (window-list f)))
                (frame-list))))

(use-package graphviz-dot-mode
  :ensure t)

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . set-buffer-variable-pitch)
  :custom
  (markdown-command "/usr/bin/markdown_py -q -x plantuml_markdown 2>/dev/null" ))

(use-package recentf
  :bind (("C-c f f" . #'recentf-open-files))
  :config
  (recentf-mode 1))

(defun mbork/diff-last-two-kills ()
  "Put the last two kills to temporary buffers and diff them."
  (interactive)
  (let ((old (generate-new-buffer "old"))
	    (new (generate-new-buffer "new")))
    (set-buffer old)
    (insert (current-kill 0 t))
    (set-buffer new)
    (insert (current-kill 1 t))
    (diff old new)
    (kill-buffer old)
    (kill-buffer new)))

(bind-key "C-c C-c C-d k" #'mbork/diff-last-two-kills)

;;--------------------------------------------------------------------------------
;; LSP playground: Start with Java

(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
              ("C-c . g d" . lsp-find-definition)
              ("C-c . g i" . lsp-find-implementation)
              ("C-c . g D" . lsp-find-declaration)
              ("C-c . g r" . lsp-find-references)
              ("C-c . g t" . lsp-find-type-definition)
              ("C-c . a" . lsp-execute-code-action)
              ("M-g l d" . lsp-find-definition)
              ("M-g l i" . lsp-find-implementation)
              ("M-g l D" . lsp-find-declaration)
              ("M-g l r" . lsp-find-references)
              ("M-g l t" . lsp-find-type-definition)))

(use-package lsp-java
                                        ;  :hook (java-mode . vh/lsp-java-hook)
  :bind* (:map java-mode-map
               ("C-c . I" . lsp-java-organize-imports)
               ("C-c . i" . lsp-java-add-import)
               ("C-c . C-i" . lsp-java-add-unimplemented-methods))
  :ensure t
  :custom ((lsp-java-java-path "/opt/java/java-15-openjdk/bin/java")
           (lsp-java-import-gradle-java-home "/opt/java/java-15-openjdk")
           (lsp-java-configuration-runtimes '[(:name "JavaSE-15"
                                                 :path "/opt/java/java-15-openjdk"
                                                 :default t)])
           ( lsp-java-vmargs '("--enable-preview")))
  :config
                                        ;
  ;; (defun vh/lsp-java-hook ()
  ;;   (lsp))
  )

;; TODO: This is still borked. I need to carry the local variableness of global arguments
;; into pop-up buffers
(defun magit-home ()
  (interactive)
  (let* ((args (cons "--git-dir=dotfiles.git" magit-git-global-arguments))
         (magit-git-global-arguments args))
    (with-current-buffer (magit-status (expand-file-name "~"))
      (make-local-variable 'magit-git-global-arguments)
      (setq 'magit-git-global-arguments args)
      )
    )
  )

;; Move to an appropriate location
(use-package windmove
  :bind (("C-c <up>" . windmove-up)
         ("C-c <down>" . windmove-down)
         ("C-c <left>" . windmove-left)
         ("C-c <right>" . windmove-right)))

(use-package kotlin-mode
  :ensure t)

(add-hook 'Info-mode-hook 'set-buffer-variable-pitch)

(defun vh/import-git (package url &optional compile-command force)
  (let* ((ext-dir (concat user-emacs-directory "external"))
         (package-dir (concat (file-name-as-directory ext-dir) package)))
    ;; Requires xapian, tclap packages installed
    (when (not (file-exists-p ext-dir)) (make-directory ext-dir))
    (when (or force
              (not (file-exists-p package-dir)))
      (message (concat "Package " package " not found. Installing."))
      (let ((buffer-name (concat "*" package " install*")))
        ;; TODO: Switch to buffer and show progress
        (with-current-buffer (get-buffer-create buffer-name)
          (cd ext-dir)
          (call-process-shell-command (concat "git clone " url " " package-dir) nil buffer-name t)
          (cd package-dir)
          (if compile-command
              (funcall compile-command)
            (byte-recompile-directory package-dir 0 t)))))
    (add-to-list 'load-path package-dir)))


(vh/import-git "notdeft" "git://github.com/hasu/notdeft.git"
               (lambda ()
                 (call-process-shell-command "make all exe" nil buffer-name t)))

(use-package notdeft
  :hook (org-mode . notdeft-note-mode)
  :commands (notdeft)
  :bind ("C-c d d" . notdeft)
  :custom ((notdeft-directories '("~/Documents/notes"))
           (notdeft-extension "org")
           (notdeft-secondary-extensions '("txt")))
  :config
  (let ((notdeft-program (expand-file-name
                          (concat user-emacs-directory
                                  "external/notdeft/xapian/notdeft-xapian"))))
    (use-package notdeft-org
      :custom (notdeft-xapian-program notdeft-program))))

(defun vh/view-video-from-url (url)
  "Watch a video from URL

Currently, we use MPV/youtube-dl to watch the video. I use 'best' format, because the youtube-dl default sometimes loses the video stream."
  (async-shell-command (format "mpv '%s'" url)))

(defun vh/listen-youtube-from-url (url)
  "Listen to the audio of a youtube video from URL

Currently, we use youtube-dl and mpv to listen to the video"
  (async-shell-command (concat "/bin/sh -c "(format "mpv --ytdl --ytdl-format=bestaudio --vo=null '%s'" url))))

(defun elfeed-view-mpv (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
	         do (elfeed-untag entry 'unread)
	         when (elfeed-entry-link entry)
	         do (vh/view-video-from-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-listen-youtube (&optional use-generic-p)
  "Youtube-feed link"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
	         do (elfeed-untag entry 'unread)
	         when (elfeed-entry-link entry)
	         do (vh/listen-youtube-from-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(use-package elfeed
  :bind (:map elfeed-search-mode-map
              ("w" . 'elfeed-view-mpv)
              ("l" . 'elfeed-listen-youtube)))

(defun shr-watch-video-at-point ()
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (when (and url
               (string-match "youtube" url))
      (vh/view-video-from-url url))))

(use-package shr
  :bind (:map shr-map
              ("w" . shr-watch-video-at-point)))

(defun update-buffer-and-window-point (buf pt)
  "Update the point for buffer and all windows of the buffer"
  (with-current-buffer buf
    (goto-char pt)
    (cl-dolist (window (get-buffer-window-list nil nil t))
      (set-window-point window pt))))

(defun buffer-indirect-buffers-list (buffer)
  "Get list of all indirect buffers of BUFFER."
  (delq nil (mapcar
             (lambda (b) (when (eq buffer (buffer-base-buffer b)) b))
             (buffer-list))))

(defun sync-indirect-buffer-points ()
  "Change all points in all related buffers to current buffer."
  (interactive)
  (let* ((pt (point))
         (cur (current-buffer))
         (base-buf (buffer-base-buffer cur)))
    (if base-buf
        (update-buffer-and-window-point base-buf pt)
      (cl-dolist (buf (buffer-indirect-buffers-list cur))
        (update-buffer-and-window-point buf pt)))))

(global-set-key (kbd "C-c C-g") #'sync-indirect-buffer-points)

(defun vh/mk-tramp-sudo-path (path)
  (let (tp)
    (if (not (tramp-tramp-file-p path))
        (concat "/sudo:root@localhost:" (expand-file-name path))
      (with-parsed-tramp-file-name path tp
        (let ((sudo-localname-part (concat "|sudo:root@" tp-host ":" tp-localname))
              (localname-part (concat ":" tp-localname "$")))
          (replace-regexp-in-string localname-part sudo-localname-part path))))))

(defun vh/mk-shell (command path eshell-p)
  "Create a an interactive command that will start a shell or an eshell in the specified directory.

COMMAND is the name of the function. PATH is the default
directory of the shell. ESHELL-P controls whether a shell or an
eshell should be started."
  `(defalias ',command
     (lambda (arg)
       (interactive "P")
       (let ((default-directory (if arg (vh/mk-tramp-sudo-path ,path) ,path))
             (buffer-name (generate-new-buffer-name
                           (concat "*" (when arg "root:")
                                   ,(concat (symbol-name command)
                                            (when eshell-p ":eshell") "*")))))
         ,(if eshell-p '(let ((eshell-buffer-name buffer-name))
                          (eshell t))
            `(shell buffer-name))))
     ,(format "Start a %s in directory %s

When ARG is present, open a root shell using sudo" (if eshell-p "eshell" "shell") path)))

(setq vh/shell-launcher-keymap (make-sparse-keymap))
(bind-key "s" #'shell vh/shell-launcher-keymap)
(bind-key "C-c S" vh/shell-launcher-keymap)
(setq vh/eshell-launcher-keymap (make-sparse-keymap))
(bind-key "s" #'eshell vh/eshell-launcher-keymap)
(bind-key "C-c s" vh/eshell-launcher-keymap)

(defmacro vh/def-ssh (command path &optional key-name)
  "Define a pair of interactive commands to start shell and eshell under the given path.

COMMAND is the base name. The commands are named COMMAND-sh and COMMAND-esh for shell and eshell.

PATH is the default directory of the shells.

KEY-NAME may be specified to bind the generated commands under
vh/shell-launcher-keymap and vh/eshell-launcher-keymap maps."
  (let ((shell-command (intern (concat (symbol-name command) "-sh")))
        (eshell-command (intern (concat (symbol-name command) "-esh"))))
    `(progn ,(vh/mk-shell shell-command path nil)
            ,(vh/mk-shell eshell-command path t)
            ,@(when key-name
                (list
                 `(bind-key ,key-name #',shell-command vh/shell-launcher-keymap)
                 `(bind-key ,key-name #',eshell-command vh/eshell-launcher-keymap))))))

(vh/def-ssh cassidy "/ssh:vedat@cassidy:" "c")

(vh/def-ssh theborg "/ssh:vedat@cassidy|ssh:vedat@192.168.50.10:" "b")

(setq-default fill-column 132)
(customize-set-variable 'display-fill-column-indicator-character ?¦)
(global-display-fill-column-indicator-mode)

;; Looks like this has been fixed.
;; (defun vh/kill-tramps ()
;;   "Kill all tramp buffers (by buffer name).

;; Sometimes projectile cannot kill or visit another project due to inaccessible tramp buffers.
;; This function kills all tramp buffers to recover."
;;   (interactive)
;;   (dolist (buf (mapcar (lambda (b)
;;                        (when (string-prefix-p "*tramp" (buffer-name b)) b))
;;                      (buffer-list)))
;;   (when buf (kill-buffer buf))))

(defun recenter-or-lock (&optional arg)
  "Toggle scroll lock with two prefix args. Otherwise call recenter-top-bottom."
  (interactive "p")
  (if (= arg 16)
      (scroll-lock-mode 'toggle)
    (call-interactively #'recenter-top-bottom arg)))

(bind-key "C-l" #'recenter-or-lock)

(use-package rg :ensure t
  :bind (("C-c f S" . rg-save-search)   ; moved from C-c f s
         ("C-c f s" . #'rg-isearch-menu))
  :custom (rg-use-transient-menu nil)
  :config
  (rg-define-search rg-dwim-project-dir
    "Search for thing at point in files matching the current file under the project root directory.

This variant allows overriding project directory."
  :query point
  :format literal
  :files current
  :dir (or (and (boundp 'rg-override-project-root) rg-override-project-root) "project"))

  (rg-enable-default-bindings "\C-cf"))

(use-package url-util :demand
  :bind ("C-c w" . #'mpv-at-point )
  :init
  (defun mpv-at-point ()
    (interactive)
    (let ((url (or (get-text-property (point) 'shr-url)
                   (url-get-url-at-point))))
      (when url
        (async-shell-command (format "mpv \"%s\"" url))))))

(use-package visual-fill-column :ensure t
  :hook (gnus-article-mode . visual-fill-column-mode)
  :hook (visual-fill-column-mode . visual-line-mode)
  )

(use-package imenu :demand
  :bind ("M-g i" . imenu))

(use-package elpher :ensure t)

(use-package bongo :ensure t
  :custom (bongo-enabled-backends '(mpv mpg123 vlc mplayer speexdec)))

;;; This is interesting.
(defun er-reinstall-package (pkg)
  (unload-feature pkg)
  (package-reinstall pkg)
  (require pkg))

(use-package eradio :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(define-key global-map (kbd "C-z") (make-sparse-keymap))

(use-package face-remap
  :init
  (defhydra hydra-zoom-all (global-map "C-z F")
    "Expand Region"
    ("-" text-scale-decrease "Zoom out")
    ("+" text-scale-increase "Zoom in")
    ("0" (text-scale-increase 0) "Reset" :exit t)
    ("v" hydra-zoom-variable/body "Zoom only variable fonts"))

  (defhydra hydra-zoom-variable (global-map "C-z f")
    ("+" (face-remap-add-relative 'variable-pitch :height 1.25) "Zoom in variable-pitch")
    ("-" (face-remap-add-relative 'variable-pitch :height 0.8) "Zoom out variable-pitch")
    ("0" (face-remap-reset-base 'variable-pitch) "Reset variable-pitch (?)" :exit t)
    ("a" hydra-zoom-all/body "Zoom all fonts")))


;; Let Gnus know Gmail search syntax
(add-hook 'gnus-started-hook
          (lambda ()
            (add-to-list 'nnir-imap-search-arguments '("gmail" . "X-GM-RAW"))))

;; `gnus-group-make-nnir-group' use Gmail search syntax *by default*.
;; You can press `G G` instead `C-u G G` instead.
(setq nnir-imap-default-search-key "gmail")

;; Thank god it's emacs! Replace twitter with nitter.
(defun vh/eww-url-remap (old-function url &rest future-args)
  (apply old-function (cons
                       (replace-regexp-in-string "https?://twitter.com" "https://nitter.42l.fr" url)
                       future-args)))

(advice-add #'eww--dwim-expand-url :around 'vh/eww-url-remap)

(use-package go-mode :demand
  :bind (:map go-mode-map
              ("C-c . d d" . #'vh/go-doc)
              ("C-c . d p" . #'vh/lookup-golang-pkg)
              ("C-c . d s" . #'vh/search-go-pkg)
              ("C-c . c" . (lambda () (interactive) (compile "go build")))
              ("C-c . t" . #'vh/go-test-package)
              ("C-c . T" . #'vh/go-test-module)
              ("C-c . i m" . #'vh/insert-current-module-for-import))
  :hook (go-mode . (lambda () (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))
  :config
  ;; TODO: define a thing for go chained symbols (pick up log.Println instead of log or Println alone)
  (defun vh/go-doc (symbol)
    "Get documentation of symbol SYMBOL using `go doc`."
    (interactive "sSymbol name: ")
    (when (string-empty-p symbol) (setq symbol (thing-at-point 'word t)))
    (shell-command (concat "go doc "  symbol)))

  (defun vh/lookup-golang-pkg (pkg)
    "Lookup package documentation.

This is useful for quickly obtaining information about internal packages"
    (interactive "sPackage name: ")
    (eww (concat "https://golang.org/pkg/" pkg)))

  (defun vh/search-go-pkg (arg keywords)
    "Search packages for keywords.

This function searches a database of packages for the keywords provided.
The argument KEYWORDS is a space separated list of terms to search for."
    (interactive "p\nsSearch for: ")
    (let ((template (if (and arg (= arg 4))
                        "https://pkg.go.dev/search?q=%s"
                      "https://golang.org/search?q=%s"))
          (terms (replace-regexp-in-string " " "+" keywords)))
      (eww (format template terms ))))

  (defun vh/go-project-root ()
    "Detect the root of the current go project, and return it."
    (when (functionp 'project-root)
      (project-root (project-current))))

  (defun vh/go-test-package (arg)
    (interactive "p")
    (let ((checkrace (eq arg 4)))
      (compile (concat "go test" (when checkrace " -race")))))

  (defun vh/go-test-module ()
    "Test the entire project"
    (interactive)
    (compile (concat "go test " (vh/go-project-root) "...")))

  (defun vh/insert-current-module-for-import ()
    "Extract go module name from go.mod, and use it"
    (interactive)
    (let ((modname (with-temp-buffer
             (let ((gomod (expand-file-name "go.mod" (vh/go-project-root))))
               (when (file-readable-p gomod)
                 (insert-file gomod)
                 (goto-char (point-min))
                 (when (search-forward-regexp "module \\(.*\\)$" nil t 1)
                   (match-string 1))
                 )))))
      (when modname (progn (push-mark)
                           (insert (format "\"%s/\"" modname))
                           (backward-char 1))))))

(use-package sly :ensure t
  :custom ((inferior-lisp-program  "sbcl")))

(use-package midnight :ensure t
  :hook  (midnight . vh/clean-vc-buffers)
  :init
  (defun vh/vc-rev-buffer-list ()
    (delq nil (mapcar (lambda (b)
                        (when (string-match "~[a-zA-Z0-9]+~$" (buffer-name b)) b))
                      (buffer-list))))

  (defun vh/clean-vc-buffers ()
    (dolist (b (vh/vc-rev-buffer-list))
      (kill-buffer b))))

(use-package notmuch
  :bind(:map notmuch-show-mode-map
             (". i d" . vh/save-calendar-to-diary))
  :config
  (defun vh/save-calendar-to-diary ()
    (interactive)
    (with-current-buffer (car (notmuch-show-current-part-handle "text/calendar"))
      (unwind-protect
          (icalendar-import-buffer)
        (kill-buffer (current-buffer))))))
;;(bind-key ". i d" #'vh/save-calendar-to-diary notmuch-show-mode-map)

(use-package dired-x)

(defun quit-other-window (arg)
  "Quit a quitable page in other window"
  (interactive "p")
  (save-selected-window
    (other-window arg)
    (quit-window)))

(bind-key "C-x 4 q" #'quit-other-window)

(use-package doc-view
  :custom
  ((doc-view-resolution 300)))

(defun pia/org-clock-in-heading (heading)
  "Clock in to a heading in gtd.org"
  (interactive)
  (save-excursion
    (org-open-file "~/org/gtd.org" nil nil heading)
    (org-clock-in)))

(defun pia/org-clock-in-team ()
  "Clock in to team support"
  (interactive)
  (pia/org-clock-in-heading "*team support"))

(defun pia/org-clock-in-unexpected ()
  "Clock in to an unexpected interruption that needs to be relabeled"
  (interactive)
  (pia/org-clock-in-heading "*Unexpected: relabel"))

(defun my/org-clock-in-interrupted ()
  "Clock in to interrupted task"
  (interactive)
  (save-excursion
    (org-goto-marker-or-bmk org-clock-interrupted-task)
    (org-clock-in)))
