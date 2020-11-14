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
  :config
  (custom-set-variables '(plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
                        '(plantuml-default-exec-mode 'jar)
                        '(org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
                        )
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
  :config
  (custom-set-variables '(markdown-command "/usr/bin/markdown_py -q -x plantuml_markdown 2>/dev/null" )))

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
  :config
  (setq lsp-java-java-path "/opt/java/java-14-openjdk/bin/java")
  (setq lsp-java-import-gradle-java-home "/opt/java/java-14-openjdk")
  (setq lsp-java-configuration-runtimes '[(:name "JavaSE-14"
                                                 :path "/opt/java/java-14-openjdk"
                                                 :default t)])
  (setq lsp-java-vmargs (list
                         "--enable-preview"))

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

(defun maybe-enable-org-roam ()
  (when (string-prefix-p (expand-file-name org-roam-directory) default-directory)
    (org-roam-mode)))

(use-package org-roam
  :ensure t
  :hook
  (org-mode . maybe-enable-org-roam)
  :bind (:map org-roam-mode-map
              (("C-c r l" . org-roam)
               ("C-c r f" . org-roam-find-file)
               ("C-c r I" . org-roam-jump-to-index)
               ("C-c r b" . org-roam-switch-to-buffer)
               ("C-c r g" . org-roam-graph))
              :map org-mode-map
              (("C-c r i" . org-roam-insert)))
  :custom
  (org-roam-directory "~/Documents/notes/"))

(use-package company-org-roam
  :ensure t
  :config
  (push 'company-org-roam company-backends))

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
  :config
  (customize-set-value 'notdeft-directories '("~/Documents/notes"))
  (customize-set-value 'notdeft-extension "org")
  (customize-set-value 'notdeft-secondary-extensions '("txt"))
  (require 'notdeft-org)
  (customize-set-value 'notdeft-xapian-program (expand-file-name (concat user-emacs-directory "external/notdeft/xapian/notdeft-xapian"))))

(defun vh/view-video-from-url (url)
  "Watch a video from URL

Currently, we use MPV/youtube-dl to watch the video. I use 'best' format, because the youtube-dl default sometimes loses the video stream."
  (async-shell-command (format "mpv --ytdl --ytdl-format=best '%s'" url)))

(defun vh/listen-youtube-from-url (url)
  "Listen to the audio of a youtube video from URL

Currently, we use youtube-dl and mpv to listen to the video"
  (async-shell-command (concat "/bin/sh -c "(format "mpv --ytdl --ytdl-format=bestaudio '%s'" url))))

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

(defun cassidy-sh ()
  (interactive)
  (let ((default-directory "/ssh:vedat@cassidy:~")
        (current-prefix-arg '(4))
        (explicit-shell-file-name "zsh"))
    (shell)))


(setq-default fill-column 132)
(customize-set-variable 'display-fill-column-indicator-character ?¦)
(global-display-fill-column-indicator-mode)

(use-package origami :ensure t
  :bind (:map origami-mode-map
              ("C-c f f" . origami-toggle-node)
              ("C-c f a" . origami-toggle-all-nodes)
              ))


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
(use-package icomplete :demand
  :config
  (setq icomplete-separator " ▪ ")
  (setq icomplete-prospects-height 1)
  (fido-mode -1)
  (icomplete-mode 1))

(use-package minibuffer :demand
  :config
  (setq completion-styles '(partial-completion substring initials flex emacs22)))


(use-package rg :ensure t
  :config
  (rg-enable-default-bindings "\C-cf"))
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

