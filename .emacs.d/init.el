;; Copyright 2017, Ahmet Vedat Hallac
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(setq custom-file "~/.emacs.d/customizations.el")
;; I don't want any customizations to longer. Anything interesting in this file
;; should be moved to the appropriate section in startup.
;; (load-file custom-file)

(require 'cl-lib)

(package-initialize)
;;; (setq package-enable-at-startup nil)

(add-to-list 'load-path "~/.emacs.d/elisp/thirdparty")
(add-to-list 'load-path "~/.emacs.d/elisp/mine")

(setq user-full-name "Vedat Hallac"
      user-mail-address "vedathallac@gmail.com")

(setq make-backup-files nil
      auto-save-default nil)

(defun recursive-directory-list (path)
  (let* ((toplevel (directory-files path t))
         (dirs '()))
    (while toplevel
      (let ((file (car toplevel)))
        (unless (member
                 (file-name-nondirectory file)
                 '("." ".." "cvs" "CVS" "rcs" "RCS" ".svn" "emacs" "xemacs" ".git"))
          (if (file-directory-p file)
              (setq dirs (append dirs
                                 (recursive-directory-list file)))))
        (setq toplevel (cdr toplevel))))
    (setq dirs (append dirs (list path)))))

(add-to-list 'package-archives '("melpa"  . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "https://tromey.com/elpa/") t)
(add-to-list 'package-archives '("gnu"    . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("orgm"   . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("copcu"  . "http://cop.cuyuz.biz/elpa/") t)
(unless (> (length package-archive-contents) 0 )
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  (package-activate 'use-package))
(require 'use-package)
;; (custom-set-variables '(use-package-verbose t)
;;                       '(use-package-always-ensure t))

(defconst upgrade-builtins-min-versions '((tramp . (2 4))
                                          (project . (0 5 3))))

(dolist (elt upgrade-builtins-min-versions)
  (let ((pkg (car elt))
        (min-ver (cdr elt)))
    (unless (package-installed-p pkg min-ver)
      (message "installing")
      (package-install (cadr (assoc pkg package-archive-contents))))))

(custom-set-variables '(erc-dcc-get-default-directory "~/erc_dcc"))

(load "term/xterm")

(defun terminal-init-screen ()
  "Terminal initialization function for screen."
   ;; Use the xterm color initialization code.
   (xterm-register-default-colors)
   (tty-set-up-initial-frame-faces))

;; Define sone terminal key codes.
;; TODO: How compatible are these between different terminals?
(define-key function-key-map "\e[1;2A" '[S-up])
(define-key function-key-map "\e[1;2B" '[S-down])
(define-key function-key-map "\e[1;2C" '[S-right])
(define-key function-key-map "\e[1;2D" '[S-left])
(define-key function-key-map "\e[1;3A" '[M-up])
(define-key function-key-map "\e[1;3B" '[M-down])
(define-key function-key-map "\e[1;3C" '[M-right])
(define-key function-key-map "\e[1;3D" '[M-left])
(define-key function-key-map "\e[1;4A" '[M-S-up])
(define-key function-key-map "\e[1;4B" '[M-S-down])
(define-key function-key-map "\e[1;4C" '[M-S-right])
(define-key function-key-map "\e[1;4D" '[M-S-left])
(define-key function-key-map "\e[1;5A" '[C-up])
(define-key function-key-map "\e[1;5B" '[C-down])
(define-key function-key-map "\e[1;5C" '[C-right])
(define-key function-key-map "\e[1;5D" '[C-left])
(define-key function-key-map "\e[1;6A" '[C-S-up])
(define-key function-key-map "\e[1;6B" '[C-S-down])
(define-key function-key-map "\e[1;6C" '[C-S-right])
(define-key function-key-map "\e[1;6D" '[C-S-left])
(define-key function-key-map "\e[13~" '[F3])
(define-key function-key-map "\e[14~" '[f4])
;; TODO: Fix these on a unix terminal. Or fix the ones above.
(define-key key-translation-map (kbd "M-[ 1 ~") (kbd "<home>"))
(define-key key-translation-map (kbd "M-[ 1 ^") (kbd "C-<home>"))
(define-key key-translation-map (kbd "M-[ 1 ; 5 H") (kbd "C-<home>"))
(define-key key-translation-map (kbd "<select>") (kbd "<end>"))
(define-key key-translation-map (kbd "M-[ 4 ~") (kbd "<end>"))
(define-key key-translation-map (kbd "M-[ 4 ^") (kbd "C-<end>"))
(define-key key-translation-map (kbd "M-[ 1 ; 5 F") (kbd "C-<end>"))

(defun match-parenthesis (arg)
  "Match the current character according to the syntax table.

   Based on the freely available match-paren.el by Kayvan Sylvan.
   I merged code from goto-matching-paren-or-insert and match-it.

   You can define new \"parentheses\" (matching pairs).
   Example: angle brackets. Add the following to your .emacs file:

    (modify-syntax-entry ?< \"(>\" )
    (modify-syntax-entry ?> \")<\" )

   You can set hot keys to perform matching with one keystroke.
   Example: f6 and Control-C 6.

    (global-set-key \"\\C-c6\" 'match-parenthesis)
    (global-set-key [f6] 'match-parenthesis)

   Simon Hawkin <cema@cs.umd.edu> 03/14/1998"
  (interactive "p")
  ;;The ?= can be anything that is not a ?\(or ?\)
  (let ((syntax (char-syntax (or (char-after) ?=)))
        (syntax2 (char-syntax (or (char-before) ?=))))
    (cond
     ((= syntax ?\() (forward-sexp 1) (backward-char))
     ((= syntax ?\)) (forward-char) (backward-sexp 1))
     ((= syntax2 ?\() (backward-char) (forward-sexp 1) (backward-char))
     ((= syntax2 ?\)) (backward-sexp 1))
     (t (message "No match")))))

(define-key global-map (kbd "M-]") 'match-parenthesis)
(define-key global-map (kbd "<f5>") 'revert-buffer)

(define-key global-map (kbd "C-x C-b") 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)

(show-paren-mode 1)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'capitalize-region 'disabled nil)

(put 'dired-find-alternate-file 'disabled nil)

(setq kill-do-not-save-duplicates t
      next-line-add-newlines nil
      require-final-newline t
      tab-always-indent 'complete)

(setq-default tab-width 4
              fill-column 80
              indent-tabs-mode nil)

(setq sentence-end-double-space nil)

(global-font-lock-mode 1)

(custom-set-variables '(pop-up-windows t))

(use-package uniquify
  :custom ((uniquify-buffer-name-style 'post-forward)
           (uniquify-separator ":")))

;; There are no scrollbars. I want to see location.
(setq line-number-mode t
      column-number-mode t)

(set-language-environment 'utf-8)

(add-hook 'after-make-frame-functions (lambda (frame)
                                        (when (window-system frame)
                                          (scroll-bar-mode -1)
                                          ;; (set-cursor-color "light green")
                                          )
                                        (blink-cursor-mode -1)
                                        (setq transient-mark-mode nil)
                                        (menu-bar-mode -1)
                                        (tool-bar-mode -1)))

;; Make sure the hooks are run if we are not in daemon mode
;; NOTE: Check if this is still necessary.
(if (not (daemonp))
    (add-hook 'after-init-hook (lambda ()
                                 (run-hook-with-args 'after-make-frame-functions
                                                     (selected-frame)))))

(defconst browse-font-large-font-size 105)

(defface large-variable-pitch `((t (:inherit 'variable-pitch :height ,browse-font-large-font-size)))
  "Font for less eye strain during prolonged reading"
  :group 'local)

(defun set-buffer-variable-pitch (&rest fixed-pitch-faces)
  (variable-pitch-mode t)
  (setq line-spacing 3)
  (when fixed-pitch-faces
    (mapcar (lambda (x) (set-face-attribute x nil :inherit 'fixed-pitch)) fixed-pitch-faces)))

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

(defun eww-browse-external ()
  (interactive)
  (shr-browse-url t))

(defun eww-browse-shr-url ()
  (interactive)
  (eww (get-text-property (point) 'shr-url)))

(use-package eww
  :bind (:map eww-link-keymap
         ("V" . eww-browse-external)))

(use-package shr
  :bind (:map shr-map
              ("v" . eww-browse-shr-url)
              ("V" . eww-browse-external)))

(use-package elfeed :ensure t
  :bind (:map elfeed-show-mode-map
              ("v" . eww-browse-shr-url)
              ("V" . eww-browse-external)))

(defun eww-browse-gnus-url ()
  (interactive)
  (eww (get-text-property (point) 'gnus-string)))

(use-package gnus-art
  :bind (:map gnus-article-mode-map
              ("v" . eww-browse-gnus-url)
              ("V" . push-button)))

(use-package find-func :demand
  :bind (("C-h C-f" . #'find-function)  ;; Replaces view-emacs-FAQ
         ("C-h C-v" . #'find-variable)
         ("C-h C-k")))

(defun quit-other-window (arg)
  "Quit the buffer in other window"
  (interactive "p")
  (save-selected-window
    (other-window arg)
    (quit-window)))

(bind-key "C-x 4 q" #'quit-other-window)

(defun wg/kludge-gpg-agent ()
  (if (display-graphic-p)
      (setenv "DISPLAY" (terminal-name))
    (setenv "GPG_TTY" (terminal-name))))

(add-hook 'window-configuration-change-hook 'wg/kludge-gpg-agent)

(defun vh/tramp-add-or-change-param (method param)
  (let ((elt (assoc method tramp-methods)))
    (when elt
      (let ((filtered-params (delq nil
                                   (mapcar (lambda (e) (when (not (eq (car e) (car param))) e))
                                           (cdr elt)))))
        (setcdr elt (push param filtered-params))))))

(use-package tramp :ensure t
  :config
  (vh/tramp-add-or-change-param "sudo" '(tramp-session-timeout 600))
  (vh/tramp-add-or-change-param "ssh" `(tramp-session-timeout ,(* 5 60 60))))

(use-package tramp-sh
  :custom ((tramp-ssh-controlmaster-options (concat
                                             "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                             "-o ControlMaster=auto -o ControlPersist=15m"))
           (tramp-use-ssh-controlmaster-options t)
           (tramp-completion-reread-directory-timeout nil))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package eshell
  :bind ("C-c s" . eshell)
  :custom  ((eshell-scroll-to-bottom-on-input 'all)
            (eshell-error-if-no-glob t)
            (eshell-hist-ignoredups t)
            (eshell-save-history-on-exit t)
            ;; Not sure about this (eshell-prefer-lisp-functions nil)
            (eshell-destroy-buffer-when-process-dies t)
            (eshell-expand-input-functions '(eshell-expand-history-references)))
  :config
  (setenv "PAGER" "cat")
  (defvar zakame/ansi-escapes-re
    (rx (or ?\233 (and ?\e ?\[))
        (zero-or-more (char (?0 . ?\?)))
        (zero-or-more (char ?\s ?- ?\/))
        (char (?@ .?~))))

  (defun zakame/nuke-ansi-escapes (beg end)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward zakame/ansi-escapes-re end t)
        (replace-match ""))))

  (defun zakame/eshell-nuke-ansi-escapes ()
    (zakame/nuke-ansi-escapes eshell-last-output-start eshell-last-output-end))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (add-to-list 'eshell-output-filter-functions
                           'zakame/eshell-nuke-ansi-escapes t)))

  (defun eshell-cd-filter (&rest args)
    (let ((dir (car (flatten-tree args))))
      (if (and dir (file-remote-p default-directory))
          (cond ((and (not (eql 0 (string-match-p "//" dir)))
                      (eql 0 (string-match-p "/" dir)))
                 (with-parsed-tramp-file-name default-directory nil
                   (list (tramp-make-tramp-file-name v dir))))
                ((eql 0 (string-match-p "//" dir))
                 (list (substring dir 1)))
                (t args))
        args)))

  (advice-add #'eshell/cd :filter-args #'eshell-cd-filter))

(use-package shell
  :bind ("C-c S" . shell))

(defun overlays-to-text ()
  "Create a new buffer called *text* containing the visible text
of the current buffer, ie. it converts overlays containing text
into real text."
  (interactive)
  (let ((tb (get-buffer-create "*text*"))
        (s (point-min))
        (os (overlays-in (point-min) (point-max))))
    (with-current-buffer tb
      (erase-buffer))
    (setq os (sort os (lambda (o1 o2)
                        (< (overlay-start o1)
                           (overlay-start o2)))))
    (mapc (lambda (o)
            (let ((bt (buffer-substring-no-properties s (overlay-start o)))
                  (b (overlay-get o 'before-string))
                  (text (or (overlay-get o 'display)
                            (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
                  (a (overlay-get o 'after-string))
                  (inv (overlay-get o 'invisible)))
              (with-current-buffer tb
                (insert bt)
                (unless inv
                  (when b (insert b))
                  (insert text)
                  (when a (insert a))))
              (setq s (overlay-end o))))
          os)
    (let ((x (buffer-substring-no-properties s (point-max))))
      (with-current-buffer tb
        (insert x)))
    (pop-to-buffer tb)))

(with-eval-after-load "ispell"
  (let ((personal-dictionary (expand-file-name "~/.config/emacs/hunspell-personal.dict"))
        (languages "en_US,en_AU,en_GB,tr_TR"))
    (setq ispell-program-name "hunspell"
          ispell-dictionary languages
          ispell-personal-dictionary personal-dictionary)
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic languages)
    (unless (file-exists-p personal-dictionary)
      (shell-command (concat "touch " personal-dictionary)))))

(use-package undo-tree :ensure t
  :init
  (global-undo-tree-mode))

(use-package expand-region :ensure t
  :after hydra
  :bind (("C-c v" . hydra-expand-region/body))
  :init
  (defhydra hydra-expand-region (:pre (activate-mark)
                                      :color red)
    "Expand Region"
    ("v" (er/expand-region 1) "Expand")
    ("V" er/contract-region "Contract")
    ("l" vh/expand-region-to-lines)
    ("C-v" (er/expand-region 0) "Reset" :color blue)
    ("s" (mark-sexp) "Sexp")
    ("t" er/mark-nxml-tag "Tag")
    ("e" er/mark-nxml-element "Element")))

(defun vh/expand-region-to-lines ()
    (interactive)
  (when (eq (point) (region-beginning))
    (exchange-point-and-mark))
  (let ((end (+ 1 (point-at-eol))))
    (exchange-point-and-mark)
    (let ((start (point-at-bol)))
      (push-mark end)
      (exchange-point-and-mark)
      (push-mark start))))

(bind-key "C-c C-v" #'vh/expand-region-to-lines)

(use-package hydra :ensure t)

(custom-set-variables '(compilation-scroll-output t))

(if (< emacs-major-version 28)
    (message "TODO: Check for emacs-28 yank-pop to see if this is obsolete yet"))
(use-package browse-kill-ring :ensure t
  :config (browse-kill-ring-default-keybindings))

(use-package paredit :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package avy :ensure t
  :bind (("M-g j" . avy-goto-char-timer)
         ("M-g w" . avy-goto-subword-1)
         ("M-g l" . avy-goto-line)
         ("C-c y l" . avy-copy-line)
         ("C-c y r" . avy-copy-region)
         ("C-c k l" . avy-kill-whole-line)
         ("C-c k r" . avy-kill-region)
         ("C-c K l" . avy-kill-ring-save-whole-line)
         ("C-c K r" . avy-kill-ring-save-region)))

(use-package imenu :demand
  :bind ("M-g i" . imenu))

(use-package dumb-jump :ensure t
  :bind (("M-g d" . xref-find-definitions)
         ("M-g D" . xref-find-definitions-other-window)))

(bind-key "M-g b" #'pop-to-mark-command)

(use-package ace-jump-buffer :ensure t
  :bind (("C-c b b"   . ace-jump-buffer)
         ("C-c b 4 b" . ace-jump-buffer-other-window)
         ("C-c b p"   . ace-jump-projectile-buffers)))

(use-package epg :ensure t
  :config
  (let ((gpg-prg "/usr/bin/gpg2"))
    (when (file-executable-p gpg-prg)
      (custom-set-variables `(epg-gpg-program ,gpg-prg)))))

(use-package auth-source
  :after epg
  :config
  (when  epg-gpg-program
    (add-to-list 'auth-sources
                 '(:source "~/.emacs.d/.secrets/authinfo.gpg" :host t :protocol t))))

(use-package dired :defer
  :custom ((dired-dwim-target t)
           (dired-listing-switches "-alt"))
  :hook  (dired-mode . (lambda ()
                         (make-local-variable 'coding-system-for-read)
                         (setq coding-system-for-read 'utf-8)
                         (gnus-dired-mode)))
  :config
  (require 'gnus-dired))

(use-package project :ensure t :pin manual)

(use-package erc
  :commands (erc erc-tls)
  :custom ((erc-dcc-get-default-directory "~/erc_dcc")
           (erc-dcc-mode t)
           (erc-dcc-verbose t)
           (erc-modules '(autojoin button completion dcc fill irccontrols
                                   list match menu move-to-prompt netsplit networks
                                   noncommands readonly ring stamp track))
           (erc-server "irc.libera.chat")))

(defun erc-libera-connect ()
  (interactive)
  ;; Pick up client certificate information from netrc
  (let ((network-stream-use-client-certificates t))
    (erc-tls :server "irc.libera.chat" :port 6697 :nick "vhallac")))

(use-package minibuffer
  :custom ((completion-styles '(orderless partial-completion))
           (completion-show-help nil))
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)
              ("M-1" . (lambda (&optional arg) (interactive "p") (vh/select-completion 1 (not (= arg 4)))))
              ("M-2" . (lambda (&optional arg) (interactive "p") (vh/select-completion 2 (not (= arg 4)))))
              ("M-3" . (lambda (&optional arg) (interactive "p") (vh/select-completion 3 (not (= arg 4)))))
              ("M-v" . (lambda () (interactive) (vh/show-completions t)))
              ("C-n" . vh/completion:first)
              ("C-p" . vh/completion:last)
              :map completion-list-mode-map
              ("C-n" . vh/completion:next)
              ("C-p" . vh/completion:prev)
              :map completion-in-region-mode-map
              ("M-1" . (lambda (&optional arg) (interactive) (vh/select-completion 1 t)))
              ("M-2" . (lambda (&optional arg) (interactive) (vh/select-completion 2 t)))
              ("M-3" . (lambda (&optional arg) (interactive) (vh/select-completion 3 t)))
              ("C-n" . vh/completion:first)
              ("C-p" . vh/completion:last))

  :config
  (defun vh/focus-minibuffer ()
    (interactive)
    (let ((minibuffer (active-minibuffer-window)))
      (when minibuffer
        (select-window minibuffer))))

  ;; NOTE: This will stop completion-in-region-mode. You need to restart it. I am closing the completion buffer
  ;; to p[rovide a hint that this is required.
  ;; The way mode can be reestablish is to advice completion-in-region to store the parameters,
  ;; and call it the same way at the end of this function when buffer is not a minibuffer (hint: minibufferp)
  (defun vh/focus-original-buffer ()
    (let ((buffer (or completion-reference-buffer (active-minibuffer-window)))
          (window (selected-window)))
      (when buffer
        (select-window (display-buffer buffer)))
      (unless (minibufferp buffer)
        (delete-window window))))

  (defun vh/show-completions (&optional focus)
    (when (not (get-buffer-window "*Completions*")) (minibuffer-completion-help))
    (when focus (select-window (display-buffer "*Completions*"))))

  (defun vh/select-completion (n &optional select)
    (vh/show-completions)
    (let ((selection (progn (switch-to-completions)
                            (goto-char 0)
                            (next-completion n)
                            (current-word t))))
      (when (or selection ())
        (if select
            (choose-completion)
          (vh/focus-minibuffer)
          (move-beginning-of-line nil)
          (kill-line)
          (insert selection)))))

  (defun vh/completion:first ()
    (interactive)
    (vh/show-completions t)
    (goto-char (point-min))
    (vh/completion:next))

  (defun vh/completion:next ()
    (interactive)
    (next-completion 1)
    (when (eq (point) (point-max))
      (goto-char (point-at-bol))
      (vh/focus-original-buffer)))

  (defun vh/completion:last ()
    (interactive)
    (vh/show-completions t)
    (goto-char (point-max))
    (vh/completion:prev))

  (defun vh/completion:prev ()
    (interactive)
    (previous-completion 1)
    (when (eq (point) (point-min))
      (vh/focus-original-buffer))))

(use-package marginalia :ensure t
  :config
  ;; This will stay here until marginalia fixes overflows in buffers
  (advice-add #'minibuffer-completion-help :after (lambda (&rest args)
                                             (with-current-buffer "*Completions*"
                                               (when (not truncate-lines)
                                                 (toggle-truncate-lines 1)))))
  (marginalia-mode 1))

(use-package orderless :ensure t
  :custom ((orderless-component-separator "[ +]")
           (orderless-matching-styles '(orderless-strict-initialism
                                        orderless-regexp
                                        orderless-prefixes
                                        orderless-literal
                                        orderless-flex))
           (orderless-style-dispatchers '(vh/orderless-flex-dispatcher)))
  :config
  (defun vh/orderless-flex-dispatcher (pattern _index _total)
    "flex style dispatcher using ! as postfix for orderless"
    (when (string-suffix-p "!" pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))))

(use-package multiple-cursors :ensure t
  :bind ( ("C-c m l" . mc/edit-lines)
          ("C-c m m" . mc/mark-more-like-this-extended)
          ("C-c m p" . mc/mark-previous-word-like-this)
          ("C-c m n" . mc/mark-next-word-like-this)
          ("C-c m P" . mc/mark-previous-symbol-like-this)
          ("C-c m N" . mc/mark-next-symbol-like-this)
          ("C-c m i" . mc/insert-numbers)
          ("C-c m s" . mc/mark-all-symbols-like-this-in-defun)
          ("C-c m S" . mc/mark-all-symbols-like-this)
          ("C-c m w" . mc/mark-all-symbols-like-this-in-defun)))

;; I sometimes use this, too
(use-package iedit :ensure t)

(use-package magit :ensure t :defer 5
  :bind (("C-c g" . magit-status))
  :init
  (use-package git-commit :ensure t :defer))

(use-package auto-complete :ensure t
  :config
  (setq-default ac-sources (push 'ac-source-yasnippet ac-sources)))

(use-package yasnippet :ensure t
  :commands (yas-minor-mode yas-global-mode yas-reload-all))

(use-package which-key :ensure t
  :config
  (which-key-mode))

(defun mk-gnus-select-method (alias addr &optional port ignore-regexp)
  "Construct an entry for `gnus-secondary-select-methods' variable.

ALIAS is the server alias. ADDR and PORT specify the server to
connect to. The optional variable IGNORE_REGEXP is copied to
gnus-ignored-newsgroups. It defaults to \"^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]\""
  `(nnimap ,alias
           (nnimap-address ,addr)
           (nnimap-server-port ,(or port 993))
           (nnimap-stream tls)
           (nnimap-list-pattern ("INBOX" "*"))
           (nnimap-expunge-on-close always)
           (gnus-check-new-newsgroups nil)
           (gnus-ignored-newsgroups ,(or ignore-regexp
                                         "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\”]\”[#’()]"))))
(use-package gnus
  :commands gnus
  :custom ((gnus-select-method '(nntp "news.easynews.com"))
           (gnus-check-new-newsgroups nil)
           (gnus-posting-styles '(((message-news-p)
                                   (name "Vedat Hallac")
                                   (address "vedat.hallac@mail.invalid"))
                                  ("gmail-2"
                                   (name "Dys@Bloodfeather")
                                   (address "dys.wowace@gmail.com"))
                                  ("gmail-android"
                                   (name "Vedat Hallac")
                                   (address "vedat@android.ciyiz.biz"))))
           (gnus-secondary-select-methods `((nntp "news.gmane.io"
                                                  (gnus-check-new-newsgroups nil))
                                            ,(mk-gnus-select-method "gmail-2" "imap.gmail.com")
                                            ,(mk-gnus-select-method "gmail-android" "imap.gmail.com")))
           (gnus-use-adaptive-scoring '(word line))
           (gnus-activate-level 3)
           (gnus-score-expiry-days 60)
           (gnus-default-adaptive-score-alist '((gnus-unread-mark)
                                                (gnus-ticked-mark (from 40))
                                                (gnus-dormant-mark (from 50))
                                                (gnus-saved-mark (from 200) (subject 50))
                                                (gnus-del-mark (from -20) (subject -50))
                                                (gnus-read-mark (from 20) (subject 10))
                                                (gnus-killed-mark (from -10) (subject -30)))))
  :config
  (setq gnus-topic-line-format "%i[ %0{%(%n (new: %A)%)%} ]\n"
        mail-user-agent 'gnus-user-agent      ; Allow Gcc:

        ;; Work-around for GMail's internal folders: When the IMAP folder contains
        ;; characters [ and ] (actually any regexp character), the function
        ;; `gnus-score-find-bnews' cannot return the ADAPT file name. This causes ADAPT
        ;; files to be generated, but not used in these groups.
        ;; The following setting ensures these two characters are never used in ADAPT
        ;; file names.
        nnheader-file-name-translation-alist '((?\[ . ?_) (?\] . ?_))
        ;; see bbdb-mua-summary-unify-format-letter configuration for bbdb for uB
        gnus-summary-line-format "%U%R%z%I%(%[%4L: %-23,23uB%]%) %s\n"
        )

  (when (require 'bbdb nil t)
    (bbdb-initialize 'gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))

  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
  (defun gnus-article-large-shr-fonts ()
    (with-current-buffer gnus-article-buffer
      (face-remap-add-relative 'variable-pitch :height browse-font-large-font-size)))

  (add-hook 'gnus-article-prepare-hook #'gnus-article-large-shr-fonts))

(use-package mm-decode :defer
  :custom ((mm-text-html-renderer 'shr)
           (mm-inline-text-html-with-images t)
           (mm-inline-large-images t)
           (mm-coding-system-priorities '(utf-8))))

(use-package notmuch :ensure t
  :commands (vh/notmuch-show-delete-thread notmuch-mua-new-mail vh/save-calendar-to-diary)
  :bind (("C-c n" . vh/hydra-notmuch-global/body)
         :map notmuch-show-mode-map
         ("K" . vh/notmuch-show-delete-thread)
         (". i d" . vh/save-calendar-to-diary))
  :after hydra
  :init
  (defhydra vh/hydra-notmuch-global (:color blue)
    "Notmuch menu"
    ("n" (notmuch) "Landing Page")
    ("m" (notmuch-mua-new-mail) "Compose mail")
    ("s" (notmuch-search) "Search mail")
    ("z" (notmuch-tree) "Search Mail (tree view)")
    ("j" (notmuch-jump-search) "Search with saved queries "))
  :custom ((notmuch-saved-searches
            '((:name "inbox.personal" :query "tag:inbox and tag:personal" :key "im")
              (:name "inbox.work" :query "tag:inbox and tag:pia" :key "ip")
              (:name "unread.personal" :query "tag:unread and tag:personal" :key "um")
              (:name "unread.work.pia" :query "tag:unread and tag:pia" :key "up")
              (:name "unread.work.qbit" :query "tag:unread and tag:qbit" :key "uq")
              (:name "flagged" :query "tag:flagged" :key "f")
              (:name "flagged-tree" :search-type tree :query "tag:flagged" :key "F")
              (:name "sent" :query "tag:sent" :key "t")
              (:name "drafts" :query "tag:draft" :key "dr")
              (:name "today" :query "dag:unread and date:today" :key "dt")
              (:name "last week" :query "date:\"this week\"" :key "dw")
              (:name "last week" :query "date:\"this month\"" :key "dm")
              (:name "all mail" :query "*" :key "a")
              (:name "info" :query "tag:info" :key "i")
              (:name "recent" :query "tag:unread and (date:yesterday or date:today)" :key "ur" :search-type tree)))

           (notmuch-archive-tags '("-inbox" "+archived"))
           (notmuch-always-prompt-for-sender t)
           (notmuch-identities '("Vedat Hallaç <vedat.hallac@pia-team.com>"
                                 "Vedat Hallaç <vedath@7island.com>"
                                 "Vedat Hallaç <vedat@hallac.net>"
                                 "Vedat Hallaç <vedat.hallac@pia-systems.com>"))
           (mm-text-html-renderer 'shr)
           (notmuch-address-use-company nil)
           (notmuch-command (expand-file-name "~/bin/remote-notmuch.sh")))
  :config
  ;; allow linking to mail from org-mode files
  (require 'ol-notmuch)

  ;; Mark deleted messages unread for fast delete
  (setcar (cdr (assoc "d" notmuch-tagging-keys)) '("+deleted" "-inbox" "-unread"))
  (push '("lf" ("+financial" "-inbox") "Financial") notmuch-tagging-keys)
  (push '("lp" ("+project" "-inbox") "Project") notmuch-tagging-keys)
  (push '("lP" ("+prospect" "-inbox") "Project") notmuch-tagging-keys)
  (push '("li" ("+info" "-inbox") "info") notmuch-tagging-keys)
  (push '("lnn" ("+notice" "-inbox") "Notice") notmuch-tagging-keys)
  (push '("lnm" ("+misc" "-inbox") "Misc") notmuch-tagging-keys)
  (push '("lnN" ("+announcement" "-inbox") "Announcement") notmuch-tagging-keys)
  (push '("lns" ("+siam" "-inbox") "SIAM") notmuch-tagging-keys)
  (push '("lna" ("+acm" "-inbox") "SIAM") notmuch-tagging-keys)
  (push '("lnb" ("+boun" "-inbox") "Boğ. Üni.") notmuch-tagging-keys)

  (defun vh/notmuch-show-delete-thread ()
    (interactive "")
    (let ((notmuch-archive-tags '("-inbox" "+deleted")))
      (notmuch-show-archive-thread-then-exit)))

  (defun vh/notmuch-address-selection-function (prompt collection initial-input)
    (let* ((from (or  (message-fetch-field "From" "")))
           (mail-addr (car
                       (delq nil (mapcar
                                  (lambda (x) (when  (string-match "@" x) x))
                                  (split-string from "[<>]")))))
           (domain (when mail-addr
                     (cadr (split-string mail-addr "@"))))
           (exists (and (delq nil (mapcar
                                   (lambda (x)  (string-match domain x))
                                   collection))
                        t)))
      (notmuch-address-selection-function prompt collection
                                          (or
                                           (and exists domain)
                                           initial-input))))

  (setq notmuch-address-selection-function #'vh/notmuch-address-selection-function)

  (defun vh/save-calendar-to-diary ()
    (interactive)
    (with-current-buffer (car (notmuch-show-current-part-handle "text/calendar"))
      (unwind-protect
          (icalendar-import-buffer)
        (kill-buffer (current-buffer))))))

(defun vh/message-edit-body-as-org ()
  "Edit the body of the message in org-mode.

When I need to send an e-mail in HTML mode, I can easily edit in org-mode, then export using vh/message-org-to-html"
  (interactive)
  (let ((old-mode major-mode)
        (body-start (save-excursion
                      (message-goto-body)
                      (point))))
    (narrow-to-region body-start (point-max))
    ;; (setq vh-message-last-mode major-mode)
    (org-mode)
    (set (make-local-variable 'vh/message-last-mode) old-mode))
  (add-hook 'org-ctrl-c-ctrl-c-final-hook 'vh/message-back-to-message))

(defun vh/message-back-to-message ()
  "You don't need to call this usually. Just hitting 'C-c C-c' should take you out"
  (interactive)
  (when (and (boundp 'vh/message-last-mode)
             vh/message-last-mode)
    (widen)
    (funcall vh/message-last-mode)
    (setq vh/message-last-mode nil)
    (remove-hook 'org-ctrl-c-ctrl-c-final-hook 'vh/message-back-to-message)
    t))

(defun vh/message-org-to-html (arg)
  (interactive "P")
  (message-goto-body)
  (save-restriction
    (narrow-to-region (point) (point-max))
    (let* ((org-html-postamble (if arg nil
                                 vh/pia-html-sig))
           (text (org-export-as 'html)))
      (kill-region (point-min) (point-max))
      (mml-generate-mime "related")
      (mml-insert-multipart "alternative")
      (mml-insert-part "text/plain")
      (yank)
      (mml-insert-part "text/html")
      (insert (concat text "\n")))))
(use-package message :defer
  :bind (:map message-mode-map
              ("C-c o" . vh/message-edit-body-as-org)
              ("C-c h" . vh/message-org-to-html)
              ("C-c s" . vh/insert-pia-html-sig))
  :custom ((message-alternative-emails (regexp-opt '("vedathallac@gmail.com"
                                                     "vedat.hallac@gmail.com"
                                                     "dys.wowace@gmail.com"
                                                     "vedat@android.ciyiz.biz"
                                                     "vedat@oyun.cuyuz.biz"
                                                     "vedathallac@yandex.com"
                                                     "vedat@hallac.net"
                                                     "vedath@7island.com"
                                                     "vedat.hallac@pia-team.com"
                                                     "vedat.hallac@pia-systems.com")))
           (send-mail-function 'smtpmail-send-it))
  :config
  (require 'smtpmail)
  (when (require 'bbdb nil t)
    (bbdb-initialize 'message)
    (bbdb-insinuate-message)

    (setq bbdb-mua-pop-up nil
          bbdb-complete-mail-allow-cycling t)))

(use-package smtpmail :defer
  :custom (mail-host-address "hallac.net")
  :config
  (setq smtp-accounts '( (ssl "vedathallac@gmail.com" "gmail-1" "smtp.googlemail.com" 587)
                         (ssl "dys.wowace@gmail.com" "gmail-2" "smtp.googlemail.com" 587)
                         (ssl "vedat@android.ciyiz.biz" "gmail-android" "smtp.googlemail.com" 587)
                         (ssl-xoauth2 "vedat.hallac@pia-team.com" "ms-pia" "smtp.office365.com" 587)
                         (ssl "vedat.hallac@pia-systems.com" "ms-piasys" "smtp.office365.com" 587)
                         (ssl "vedat@hallac.net" "hallac-net" "smtp.yandex.com" 587)
                         (ssl "vedath@7island.com" "gmail-qbit" "smtp.googlemail.com" 587)))
  (use-package gnutls
    :custom (gnutls-min-prime-bits 1024))
  
  ;;; This only works for emacs 24 and (hopefully) above
  (defun set-smtp-common (alias server port &optional user password)
    ;; TODO: I need both alias and real server entries in my authinfo
    ;; for this method. I don't like it. Need a better way to handle it.
    (unless user
      (setq user (plist-get (car (auth-source-search :host alias
                                                     :port 587))
                            :user)))
    (setq smtpmail-smtp-user user
          smtpmail-smtp-server server
          smtpmail-smtp-service port))
  
  (defun set-smtp (mech alias server port &optional user password)
    "Set related SMTP variables for supplied parameters."
    (set-smtp-common alias server port user password)
    (setq smtpmail-auth-supported '(cram-md5 plain login)
          smtpmail-starttls-credentials nil))
  
  (defun set-smtp-ssl (alias server port &optional user password key cert)
    "Set related SMTP and SSL variables for supplied parameters."
    (set-smtp-common alias server port user password)
    (setq starttls-use-gnutls nil        ;use starttls-program
          starttls-extra-arguments nil
          smtpmail-starttls-credentials (list (list server port key cert))
          smtpmail-auth-supported '(cram-md5 plain login)))
  
  (defun set-smtp-xauth2-ssl (alias server port &optional user password key cert)
    "Set related SMTP and SSL variables for supplied parameters."
    (set-smtp-common alias server port user password)
    (setq starttls-use-gnutls nil        ;use starttls-program
          starttls-extra-arguments nil
          smtpmail-starttls-credentials (list (list server port key cert))
          smtpmail-auth-supported '(xoauth2)))
  
  ;; From https://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00156.html
  ;; Modified to handle M$ XOAUTH2 authentication
  (cl-defmethod smtpmail-try-auth-method (process (_mech (eql xoauth2)) user password)
    ;; If the password is a token, search for it in authinfo, and use information
    ;; to obtain an access token
    (let ((tokeninfo (car (auth-source-search :host "localhost" :port "xoauth2" :user password :max 1))))
      (when tokeninfo
        (setq password (oauth2-get-refresh-token (plist-get tokeninfo :url)
                                                 (plist-get tokeninfo :clientid)
                                                 (plist-get tokeninfo :refreshtoken))))
      (smtpmail-command-or-throw process "AUTH XOAUTH2" 334)
      (smtpmail-command-or-throw process (concat
                                          (base64-encode-string
                                           (format "user=%s\001auth=Bearer %s\001\001"
                                                   (nnimap-quote-specials user)
                                                   (nnimap-quote-specials password)) t))
                                 235)))
  
  (defun oauth2-get-refresh-token (url client-id refresh-token)
    (let ((token (oauth2-get-refresh-token-json url client-id refresh-token)))
      (string-match "\"access_token\": *\"\\([^\"]*\\)\"" token)
      (match-string 1 token)))
  
  (defun oauth2-get-refresh-token-json (url client-id refresh-token)
    (let* ((url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded")))
           (grant-type "refresh_token")
           (url-request-data (format "grant_type=%s&client_id=%s&refresh_token=%s"
                                     grant-type client-id refresh-token)))
      (with-current-buffer
          (url-retrieve-synchronously url)
        (buffer-string))))
  
  (defun change-smtp ()
    "Change the SMTP server according to the current from line."
    (save-excursion
      (let ((case-fold-search t))
        (cl-loop with from = (save-restriction
                               (message-narrow-to-headers)
                               (message-fetch-field "from"))
                 for (auth-mech address . auth-spec) in smtp-accounts
                 when (string-match address from)
                 do (cond
                     ((memq auth-mech '(cram-md5 plain login))
                      (cl-return (apply 'set-smtp 'auth-mech auth-spec)))
                     ((eql auth-mech 'ssl)
                      (cl-return (apply 'set-smtp-ssl auth-spec)))
                     ((eql auth-mech 'ssl-xoauth2)
                      (cl-return (apply 'set-smtp-xauth2-ssl auth-spec)))
                     (t (error "Unrecognized SMTP auth. mechanism: `%s'" auth-mech)))
                 finally (error "Cannot infer SMTP information")))))
  
  (defadvice smtpmail-via-smtp (around set-smtp-server-from-sender activate)
    "When sending smtp mail, replace credentials according to to From: field"
    ;; Not sure if this is the right way, but it seems to prevent the password
    ;; lingering around in the variable.
    (let ((smtpmail-auth-credentials nil))
      (with-current-buffer smtpmail-text-buffer
        (change-smtp))
      ad-do-it)))

(use-package smtp-openssl :ensure t)

(use-package tls
  :requires smtp-openssl :defer
  :custom (tls-program `(,(concat
                             (if (boundp 'openssl-prg)
                                 openssl-prg
                               "openssl")
                             " s_client -connect %h:%p -no_ssl2 -ign_eof")))
  :config
  (require 'smtp-openssl))

(use-package bbdb :ensure t :defer
  :custom ((bbdb-update-records-p 'query)
           (bbdb-mua-update-interactive-p '(search . query))
           ;; Uncommenting the following allows me to auto-capture e-mails into BBDB
           ;; (bbdb-accept-message-alist '( ("From" . "@pia-team\.com")
           ;;                                ("From" . "@\\(?:milleni\\|turkcell\\)\.com\.tr")))

           ;; use %uB for names in gnus-summary-line-format configuration
           (bbdb-mua-summary-unify-format-letter "B"))
  :config
  (setq bbdb/gnus-score-default 10))

(use-package notmuch-forget
  :init
  (with-eval-after-load "notmuch-address" (notmuch-forget-install)))

(use-package csharp-mode :ensure t
  :commands (csharp-mode)
  :mode ("\\.cs" . csharp-mode)
  :hook (csharp-mode . vh/csharp-mode-func)
  :bind* (:map csharp-mode-map
               ("C-c . R" . #'vh/dotnet--restore)
               ("C-c . ." . #'vh/dotnet--build)
               ("C-c . T" . #'vh/dotnet--test))
  :config
  (defun vh/csharp-mode-func ()
    (subword-mode 1))

  (defun vh/dotnet--project-root ()
    (locate-dominating-file default-directory
                            (lambda (parent) (directory-files parent nil ".*\\.csproj"))))

  (defun vh/dotnet--run-dotnet (sub &rest args)
    "Run the dotnet utility.

SUB is the sub command. ARGS are additional arguments, if any"
    (interactive "Msub command:")
    (let ((default-directory (vh/dotnet--project-root)))
      (compile (concat "ncdotnet " sub
                       (apply #'concat args)))))

  (defmacro vh/define-dotnet-command (subcommand)
    "Defines a command VH/DOTNET--,SUBCOMMAND

The command will invoke the specified subcommand in the project directory"
    (let ((fname (intern (concat "vh/dotnet--" subcommand))))
      `(defun ,fname  (&rest args)
         (interactive)
         (apply #'vh/dotnet--run-dotnet (cons ,subcommand args)))))

  (vh/define-dotnet-command "build")
  (vh/define-dotnet-command "test")
  (vh/define-dotnet-command "restore"))

(use-package omnisharp :ensure t
  :commands (omnisharp-mode)
  :hook (csharp-mode . vh/omnisharp-csharp-func)
  :bind* (:map omnisharp-mode-map
               ("M-<RET>" . #'omnisharp-run-code-action-refactoring)
               ("C-c . <RET>" . #'omnisharp-run-code-action-refactoring)
               ("C-c . r" . #'omnisharp-rename)
               ("C-c . t" . #'vh/omnisharp-unit-test-at-point)
               ("C-c . g d" . #'omnisharp-go-to-definition)
               ("C-c . g D" . #'omnisharp-go-to-definition-other-window)
               ("C-c . R" . #'omnisharp-run-code-action-refactoring)
               ("C-c . U" . #'omnisharp-fix-usings)
               ("C-c . g u" . #'omnisharp-find-usages)
               ("C-c . g i" . #'omnisharp-find-implementations)
               ("C-c . C-i" . #'omnisharp-auto-complete)
               )
  :custom (omnisharp-server-executable-path "~/.emacs.d/.cache/omnisharp/server/v1.34.5/run")
  :config
  (defun vh/omnisharp-csharp-func ()
    (omnisharp-mode)
    (auto-complete-mode))

  (defun vh/omnisharp--namespace-of (stack)
    (-let (((&alist 'Kind kind
                    'Name name) (car stack)))
      (when (not (equal "namespace" kind))
        (error (concat "Expected namespace, got " kind)))
      name))

  (defun vh/omnisharp--class-of (stack)
    (let ((last-class (last (delq nil
                                  (mapcar (lambda (x)
                                            (-let (((&alist 'Kind kind) x))
                                              (when (equal kind "class") x))) stack)))))
      (-let (((&alist 'Name name) (car last-class)))
        name)))

  (defun vh/omnisharp--method-of (stack)
    (-let (((&alist 'Kind kind
                    'Name name) (car (last stack))))
      (when (equal "method" kind) name)))

  (defun vh/omnisharp-unit-test-at-point ()
    "Runs test case under point, if any."
    (interactive)
    (omnisharp--cs-element-stack-at-point
     (lambda (stack)
       (let ((default-directory (vh/dotnet--project-root))
             (method (vh/omnisharp--method-of stack)))
         (compile (concat "ncdotnet test --filter="
                          (vh/omnisharp--namespace-of stack)
                          "."
                          (vh/omnisharp--class-of stack)
                          (when method
                            (concat
                             "."
                             (vh/omnisharp--method-of stack))))))))))

(use-package mvn :ensure t
  :after cc-mode
  :commands (mvn-test-defun mvntest-class)
  :bind (:map java-mode-map
              ("C-c t c" . mvn-test-class)
              ("C-c t f" . mvn-test-defun))
  :init
  ;; Neither clutter nor color from mvn, please
  (defun vh/mvn--plain-output (&optional old-function task args &rest future-args)
    (apply old-function task (concat "-q -B " args) future-args))

  (advice-add #'mvn :around #'vh/mvn--plain-output)

  :config
  (defun mvn-test-class ()
    (interactive)
    (let* ((file-name (buffer-file-name))
           (class-name (car (split-string
                             (car (last (split-string file-name "/")))
                             "\\.")))
           (root (projectile-project-root)))
      (mvn-test class-name)))

  ;; prompts for a single test case in the current class and runs it

  (defun java-method-name ()
    (require 'imenu)
    (imenu--menubar-select imenu--rescan-item)
    (save-excursion
      (let ((beg-pt (progn (beginning-of-defun)
                            (point)))
            (end-pt (progn (end-of-defun)
                           (point))))
        (car (delq nil
                   (mapcar (lambda (x) (let ((pos (cdr x)))
                                         (when (and
                                                (>= pos beg-pt) (<= pos end-pt))
                                           (car x))))
                           (imenu--make-index-alist)))))))

  (defun mvn-test-defun ()
    (interactive)
    (require 'imenu)
    (let* ((file-name (buffer-file-name))
           (class-name (car (split-string
                             (car (last (split-string file-name "/")))
                             "\\.")))
           (test-case (java-method-name))
           (root (projectile-project-root))
           (mvn-cmd (concat "cd " root " && "
                            "mvn -Dtest=" class-name "#" test-case " test ")))
      (mvn-test (concat class-name (when test-case (concat "#" test-case)))))))

(use-package groovy-mode :ensure t
  :commands groovy-mode
  :mode ("\\.gradle$" . groovy-mode))

(use-package ruby-mode
  :commands ruby-mode
  :after auto-complete
  :mode ("\\(?:\\.\\(?:gemspec\\|r\\(?:ake\\|[ub]\\)\\)\\|Gemfile\\)\\$" . ruby-mode)
  :bind (:map ruby-mode-map
              ("C-x C-t" . ruby-compilation-this-rspec)
              ;;("C-c C-d" . yari-anything)
              ("#" . ruby-electric-strparam)
              ("C-M-u" . ruby-goto-containing-block-start)
              ("C-c b" . ruby-flip-containing-block-type))
  :config
  (require 'ruby-helper)
  (autoload 'word-at-point "thingatpt.el")

  (require 'auto-complete-config)

  (require 'align)
  (defconst align-ruby-modes '(ruby-mode))
  (defconst ruby-align-rules-list
    '((ruby-comma-delimiter
       (regexp . ",\\(\\s-*\\)[^/ \t\n]")
       (modes  . align-ruby-modes)
       (repeat . t))
      (ruby-symbol-after-func
       (regexp . "^\\s-*\\w+\\(\\s-+\\):\\w+")
       (modes  . align-ruby-modes))))
  (add-to-list 'align-perl-modes 'ruby-mode)
  (add-to-list 'align-dq-string-modes 'ruby-mode)
  (add-to-list 'align-sq-string-modes 'ruby-mode)
  (add-to-list 'align-open-comment-modes 'ruby-mode)
  (dolist (it ruby-align-rules-list)
    (add-to-list 'align-rules-list it))

  (add-hook 'ruby-mode-hook (lambda ()
                              (auto-complete-mode t)
                              ;; Auto-complete fixups
                              (make-local-variable 'ac-ignores)
                              (add-to-list 'ac-ignores "end")))

  (defun vh/projectile-test-prefix (orig-fun project-type &rest args)
    (let ((val
           (or (cond
                ((member project-type '(ruby)) "test_"))
               (apply orig-fun project-type args))))
      val
      ))
  (advice-add 'projectile-test-prefix :around #'vh/projectile-test-prefix))

(use-package inf-ruby :ensure t)

(use-package rake :ensure t
  :after projectile
  :config
  (projectile-register-project-type 'ruby '("Rakefile")
                                    :compile "rake"
                                    :test "rake test"))

(use-package rbenv :ensure t
  :config
  (let ((path (getenv "PATH")))
    (when (not (string-match-p "\\.rbenv/shims" path))
      (setenv "PATH" (concat path path-separator (expand-file-name "~/.rbenv/shims"))))))

(use-package bundler :ensure t)

(use-package lisp-mode
  :after paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package elisp-helper
  :bind (("C-c e" . vh-eval-and-replace)))

(use-package scheme
  :commands scheme-mode
  :mode ("\\.s\\(s\\|c[mh]\\)$" . scheme-mode))

(use-package python
  :commands python-mode
  :requires auto-complete
  :mode  ("\\.py$" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (require 'auto-complete)
  (require 'auto-complete-config)

  (add-hook 'python-mode-hook
            (lambda ()
              (custom-set-variables '(ropemacs-enable-autoimport t)
                                    ;; Automatically save project python buffers before refactorings
                                    '(ropemacs-confirm-saving 'nil))
              (unless (featurep 'ropemacs)
                (pymacs-load "ropemacs" "rope-" t)
                (ropemacs-mode 1))
              (auto-complete-mode 1)
              (ac-ropemacs-setup))))

(use-package pymacs
  :commands pymacs-load)

(use-package virtualenv :ensure t
  :commands virtualenv-activate
  :config (defvar virtualenv-use-ipython nil))

(use-package js2-mode :ensure t
  :requires yasnippet
  :mode ("\\.js\\'" . js2-mode)
  :custom (js2-indent-switch-body t)
  :config
  (yas-reload-all)
  (add-hook 'js2-mode-hook #'yas-minor-mode-on)
  ;; This is for jasmine output. But it needs more work
  (add-to-list 'compilation-error-regexp-alist '("^\\W+at\\(.*\\)\\ (\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 2 3 4)))

(use-package lua-mode :ensure t
  :commands lua-mode
  :hook  (lua-mode . (lambda ()
                       (setq lua-electric-mode nil
                             lua-indent-level 4)
                       ;; (choose-indent-type)
                       (auto-fill-mode 1)
                       (subword-mode 1))))

(use-package go-mode :ensure t)

(use-package cc-mode
  :bind (:map c-mode-map
              ("C-c C-f" . c-helper-find-file)
              ("C-c C-v" . c-helper-find-include-file))
  
  
  :custom ((c-echo-syntactic-information-p t)
           (c-electric-pound-behavior '(alignleft))
           (c-indent-comments-syntactically-p t))
  :config
  (setq c-macro-shrink-window-flag t)
  (add-hook 'c-mode-common-hook (lambda ()
                                  (auto-fill-mode t)
                                  (auto-complete-mode)))
  (require 'etags)
  
  (defvar c-helper-find-file-history nil)
  (defvar c-helper-global-search-list nil)
  (defvar c-helper-buffer-specific-dir-hook nil)
  
  (defun c-helper-find-file (&optional filename)
    "Finds the file in the current include path.
  See c-helper-include-path for the current include path."
    (interactive)
    (progn
    (if (or (not filename)
        	  (eq (string-width filename) 0))
        (setq filename (read-string "Please enter the file name: "
                                      ""
                                      'c-helper-find-file-history
                                      "")) )
    (let ((dirs (append c-helper-global-search-list
                          (if (functionp c-helper-buffer-specific-dir-hook)
                              (funcall c-helper-buffer-specific-dir-hook)
                            nil))))
                                          ; Try to find in the tag list, if appropriate
      (if (buffer-tag-table-list)
        	(let ((fname (c-helper-find-in-tags filename)))
        	  (if fname
        		  (progn
                    (if (> (count-windows) 1)
                        (find-file-other-window fname)
                      (find-file fname))
                    (return nil)))))
  
                                          ; Otherwise, try the specified directories
      (if dirs
        	(let ((fname (c-helper-find-under-dirs dirs filename)))
        	  (if fname
        		  (if (> (count-windows) 1)
        			  (find-file-other-window fname)
        			(find-file fname))
        		(error (concat "Cannot find file: " filename))))
        (error "Cannot construct search path")))))
  
  
  (defun c-helper-find-in-tags (filename)
    "Locates a file in the buffer's tag files.
  Returns the absolute path to the file, if found in the TAGS list,
  otherwise return nil."
    (let ((files (buffer-tag-table-files))
          (name nil))
      (while (and files (null name))
        (if (partial-file-path-match (car files) filename)
            (setq name (car files)))
        (setq files (cdr files)))
      (if name
          (expand-file-name name))))
  
  (defun c-helper-find-under-dirs (dirlist filename)
    "Locate the file under DIRLIST.
  If the same file appears more than once in the directory list, the one closest
  to the top list of directories is found."
    (let ((name nil))
      (while dirlist
        (let* ((dir (car dirlist))
               (contents (directory-files dir t))
               (files nil)
               (dirs nil))
          (mapc #'(lambda (name)
                   (cond ((and (file-directory-p name)
                               (not (member
                                     (file-name-nondirectory name)
                                     '("." ".." "cvs" "CVS" "rcs" "RCS" ".svn"))))
                          (setq dirs (cons name dirs)))
                         ((and (not (file-directory-p name))
                               (file-readable-p name))
                          (setq files (cons (convert-standard-filename name) files))))
                   nil)
                contents)
          (while (and files (null name))
            (if (partial-file-path-match (car files) filename)
                (setq name (car files)))
            (setq files (cdr files)))
          (setq dirlist (append (cdr dirlist) dirs)))
        (if name
            (setq dirlist nil)))
      name))
  
  (defun partial-file-path-match (full-path partial-path)
    "Compare a full (at least fuller) path against a sub-path.
  If the trailing parts of two paths match, returns t. Otherwise, returns nil.
  For example \"/usr/local/bin/emacs\" vs \"bin/emacs\" returns t."
    (let ((match t))
      (while (and match partial-path)
        (let ((full-last (file-name-nondirectory full-path))
              (partial-last (file-name-nondirectory partial-path)))
          (if (or (null partial-last)
                  (string-equal partial-last ""))
              (setq partial-path nil)
            (setq match (string-equal full-last partial-last))
            (setq full-path (file-name-directory full-path))
            (setq partial-path (file-name-directory partial-path))
            (if full-path
                (setq full-path (directory-file-name full-path)))
            (if partial-path
                (setq partial-path (directory-file-name partial-path))))))
      match))
  
  
  (defun c-helper-find-include-file ()
    "Extracts the include file from the line under the point,
  and finds it in the search path."
    (interactive)
    (save-excursion
    (beginning-of-line)
    (if (search-forward-regexp "#include\\s-*[\\\"<]\\(.*\\)[\\\">]"
        						 (point-at-eol) ; limit
        						 t ; noerror
        						 )
        (let ((file-name (buffer-substring-no-properties
                            (match-beginning 1) (match-end 1))))
        	(if file-name
        		(c-helper-find-file file-name)
        	  (error "No file specified in the #include statement")))
      (error "Not on a line with a #include statement"))))
  (c-add-style "tda" '((c-basic-offset . 4)
                       (c-comment-only-line-offset . 0)
                       (c-block-comment-prefix . "*")
                       (c-hanging-braces-alist     . ((substatement-open        before after)
                                                      (brace-list-open          after)
                                                      (brace-list-intro)
                                                      (brace-entry-open         before)
                                                      (brace-list-close  . vh/c-snug-array-close)
                                                      (block-close       . c-snug-do-while)
                                                      (class-open               after)
                                                      (class-close              before)))
                       (c-hanging-colons-alist     . ((case-label after)
                                                      (label after)
                                                      (member-init-intro before)
                                                      (inher-intro)))
                       (c-offsets-alist . ((topmost-intro         . 0)
                                           (topmost-intro-cont    . 0)
                                           (substatement          . +)
                                           (substatement-open     . 0)
                                           (case-label            . 0)
                                           (label                 . 0)
                                           (access-label          . -)
                                           (inclass               . +)
                                           (inline-open           . 0)
                                           (cpp-macro-cont        . ++)
                                           (arglist-intro         . c-lineup-arglist-intro-after-paren)
                                           (arglist-cont          . c-lineup-arglist)
                                           (arglist-cont-nonempty . c-lineup-arglist)
                                           (arglist-close         . c-lineup-arglist)
                                           (inextern-lang         . -)
                                           (statement-cont        . vh/c-lineup-array-init)))
                       (c-cleanup-list . (empty-defun-braces
                                          list-close-comma
                                          scope-operator
                                          one-liner-defun
                                          comment-close-slash))
                       (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))
  
  (c-add-style "eracom" '((c-basic-offset . 4)
                          (c-comment-only-line-offset . 0)
                          (c-block-comment-prefix . "*")
                          (c-hanging-braces-alist     . ((substatement-open        before after)
                                                         (brace-list-open          after)
                                                         (brace-list-intro)
                                                         (brace-entry-open         before)
                                                         (brace-list-close  . vh/c-snug-array-close)
                                                         (block-close       . c-snug-do-while)
                                                         (class-open               after)
                                                         (class-close              before)))
                          (c-hanging-colons-alist     . ((case-label after)
                                                         (label after)
                                                         (member-init-intro before)
                                                         (inher-intro)))
                          (c-offsets-alist . ((topmost-intro         . 0)
                                              (topmost-intro-cont    . 0)
                                              (substatement          . +)
                                              (substatement-open     . 0)
                                              (case-label            . 0)
                                              (label                 . 0)
                                              (access-label          . -)
                                              (inclass               . +)
                                              (inline-open           . 0)
                                              (cpp-macro-cont        . ++)
                                              (arglist-intro         . c-lineup-arglist-intro-after-paren)
                                              (arglist-cont          . c-lineup-arglist)
                                              (arglist-cont-nonempty . c-lineup-arglist)
                                              (arglist-close         . c-lineup-arglist)
                                              (inextern-lang         . -)
                                              (statement-cont        . vh/clineup-array-init)))
                          (c-cleanup-list . (empty-defun-braces
                                             list-close-comma
                                             scope-operator))
                          (c-hanging-semi&comma-criteria . (c-semi&comma-inside-parenlist))))
  
  (c-add-style "eracom-old" '((c-basic-offset . 4)
                              (c-comment-only-line-offset . 0)
                              (c-block-comment-prefix . "*")
                              (c-hanging-braces-alist     . ((substatement-open after)
                                                             (brace-list-open   after)
                                                             (brace-list-intro)
                                                             (brace-entry-open  after)
                                                             (brace-list-close  before)
                                                             (block-close       . c-snug-do-while)
                                                             (class-open        after)))
                              (c-hanging-colons-alist     . ((case-label after)
                                                             (label after)
                                                             (member-init-intro before)
                                                             (inher-intro)))
                              (c-offsets-alist . ((topmost-intro         . 0)
                                                  (topmost-intro-cont    . 0)
                                                  (substatement          . +)
                                                  (substatement-open     . 0)
                                                  (case-label            . 0)
                                                  (label                 . 0)
                                                  (access-label          . -)
                                                  (inclass               . +)
                                                  (inline-open           . 0)
                                                  (cpp-macro-cont        . ++)
                                                  (arglist-intro         . c-lineup-arglist-intro-after-paren)
                                                  (arglist-cont          . c-lineup-arglist)
                                                  (arglist-cont-nonempty . c-lineup-arglist)
                                                  (arglist-close         . c-lineup-arglist)))
                              (c-cleanup-list . (brace-else-brace
                                                 brace-elseif-brace
                                                 empty-defun-braces
                                                 list-close-comma
                                                 scope-operator))))
  
  
  (defun vh/c-snug-array-close (syntax pos)
    "Dynamically calculate close-brace hanginess for array initializations.
  
  See `c-hanging-braces-alist' for how to utilize this function as an
  ACTION associated with `brace-list-close' syntax."
    (save-excursion
      (if (eq syntax 'brace-list-close)
          (match-parenthesis 0))
      (c-safe (c-forward-token-1 -1))
      (if (eq (char-after) ?\=)
          '(before)
        '(after))))
  
  (defun vh/c-lineup-array-init (langelem)
    "Correct the indentation of array and structure initializer brace, when it is
  reported as statement-cont.
  
  Changes:
  int a[] =             int a[] =
     {                  {
        1,2,3      ->      1,2,3
     };                 };"
    (let ((default-lineup (c-lineup-math langelem)))
      (save-excursion
        (goto-char (point-at-bol))
        (if (and (looking-at "\\s-*{")
                 (progn (c-safe (c-backward-token-1 1))
                        (eq (char-after) ?\=)))
            0
          default-lineup))))
  (add-hook 'c-mode-hook (lambda ()
                           (c-set-style "tda")))
  (c-add-style "java-custom"
               '("java"
                 (c-offsets-alist . ((substatement-open . 0)
                                     (arglist-cont-nonempty . (c-lineup-cascaded-calls
                                                               c-lineup-argcont))
                                     (statement-cont . (c-lineup-cascaded-calls
                                                        c-lineup-assignments))))
                 (c-hanging-braces-alist . ((class-open after)
                                            (inexpr-class-open after)
                                            (inexpr-class-close before)
                                            (defun-open after)
                                            (inline-open after)
                                            (substatement-open after)
                                            (block-close . c-snug-do-while)))))
  
  
  (c-add-style "java-google"
               '("java"
                 (c-tab-always-indent        . t)
                 (c-basic-offset . 2)
                 (c-ignore-auto-fill . nil)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist     . ((class-open after)
                                                (inexpr-class-open after)
                                                (inexpr-class-close before)
                                                (defun-open after)
                                                (inline-open after)
                                                (substatement-open after)
                                                (block-close . c-snug-do-while)
                                                (brace-list-open)))
                 (c-hanging-colons-alist     . ((member-init-intro before)
                                                (inher-intro)
                                                (case-label after)
                                                (label after)
                                                (access-label after)))
                 (c-cleanup-list             . (scope-operator
                                                brace-else-brace
                                                brace-elseif-brace
                                                brace-catch-brace
                                                empty-defun-braces
                                                defun-close-semi))
                 (c-offsets-alist . ((knr-argdecl-intro . 5)
                                     (arglist-intro . +)
                                     (arglist-close . c-lineup-close-paren)
                                     (inclass . +)
                                     (member-init-intro . +)
                                     (statement-block-intro . +)
                                     (defun-block-intro . +)
                                     (substatement-open . 0)
                                     (label . 0)
                                     (statement-case-open . +)
                                     (statement-case-intro . +)
                                     (case-label . 0)
                                     (statement-cont . c-lineup-math)
                                     (inline-open . 0)
                                     (brace-list-open . +)
                                     (topmost-intro-cont . 0)
                                     (c . 1) ; "c" for continue of comment, not "c
                                          ; programming language"
                                     ))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "lgf: ")
                 (c-comment-prefix-regexp . ((awk-mode . "#+(lgf: )?")
                                             (other ."lgf: \\|//+\\|\\**")))
                 ;; go to this file and test if c block comments works
                 ;; [[file:./patches/comments-test.c]]
                 (c-echo-syntactic-information-p . t)))
  (add-hook 'java-mode-hook (lambda ()
                              (subword-mode)
                              (c-toggle-auto-newline 1)
                              (setq c-cleanup-list 'set-from-style)
                              (c-set-style "java-google")
                              (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))))

(use-package flycheck :ensure t
  :bind (:map flycheck-mode-map ("C-c ! !" . org-time-stamp-inactive))
  :init
  (global-flycheck-mode)
  :custom (flycheck-temp-prefix "#flycheck")
  :config
  (add-hook 'js2-mode-hook (lambda nil
                             (add-to-list 'flycheck-disabled-checkers 'javascript-jshint)))
  ;;; Use "sudo npm install -g eslint"
  (custom-set-variables '(flycheck-javascript-eslint-executable "/usr/local/bin/eslint"))
  (custom-set-variables '(flycheck-emacs-lisp-load-path 'inherit))
  (add-hook 'emacs-lisp-mode-hook (lambda nil
                                    (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc))))

(let* ((flycheck-java-dir "~/.emacs.d/elisp/thirdparty/flycheck-java")
       (bin-dir "~/.emacs.d/bin")
       (ecj-jar-file (when (file-directory-p bin-dir)
                       (car (last (directory-files  bin-dir t "ecj.*jar"))))))
  (when (and ecj-jar-file
             (file-exists-p flycheck-java-dir))
    (setq load-path (cons flycheck-java-dir load-path))
    (use-package flycheck-java
      :config
      (setq flycheck-java-ecj-jar-path ecj-jar-file))))

(use-package haskell-mode :ensure t
  :custom ((flycheck-ghc-args "-dynamic")
           (haskell-compile-command "ghc -dynamic -Wall -ferror-spans -fforce-recomp -c %s")))

(defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

(defun yaml-mode-outline-hook ()
    (outline-minor-mode)
    (setq outline-regexp "^\\([ ]\\{2\\}\\)*\\([-] \\)?\\([\"'][^\"']*[\"']\\|[a-zA-Z0-9_-]*\\|/[^:]*\\): *\\([>|]\\|&[a-zA-Z0-9_-]*\\)?$")
    (setq outline-level 'yaml-outline-level))

(use-package yaml-mode :ensure t
  :hook (yaml-mode . (lambda ()
                       (subword-mode)
                       (auto-fill-mode)
                       (yaml-mode-outline-hook)))
  :bind (:map yaml-mode-map
         ("C-<tab>" . outline-toggle-children)))

(use-package nxml-mode
  :commands nxml-mode
  :bind (:map nxml-mode-map
              ("C-c k c" . comment-region)
              ("C-c k i" . indent-xml-file))
  :mode ("\\.\\(x[ms]l\\|rng\\|x?html?\\)\\'" . nxml-mode)
  :config
  (setq nxml-child-indent 4
        nxml-outline-child-indent 4
        nxml-slash-auto-complete-flag nil)
  (defun indent-xml-file ()
    "Indent entire XML file"
    (interactive "")
    (shell-command-on-region (point-min) (point-max) "xmlindent" (current-buffer) t))

  (add-hook 'nxml-mode-hook
            #'(lambda ()
               ;; (choose-indent-type)
               ;; Add my schema files to RNG search path
               (add-to-list 'rng-schema-locating-files
                            "~/.emacs.d/nxml-schemas/schemas.xml")
               (add-to-list 'rng-schema-locating-files
                            "~/.emacs.d/nxml-schemas/libvirt/schemas.xml"))))

(use-package org :ensure org-plus-contrib :defer
  :hook (org-mode . (lambda ()
                      (auto-fill-mode)))
  :bind (("C-c b o"   . org-switchb)
         ("C-c b 4 o" . org-switch-to-buffer-other-window)
         ("C-c l"     . org-store-link))
  :init
  (defun vh/revert-org-insert-heading-arg-behavior (&optional old-function arg
                                                              invisible-ok top &rest future-args)
    "Revert behavior of M-RET.

When arg is not provided, respect content; when it is 4, insert heading
immediately after current heading."
    (message (if (not  arg) "nil"))
    (let ((arg (cond ((equal arg '(4)) nil)
                     ((not arg) '(4))
                     (t nil))))
      (message "coo")
      (message (if (not  arg) "nil"))
      (apply old-function arg invisible-ok top future-args)))

  (advice-add #'org-insert-heading :around #'vh/revert-org-insert-heading-arg-behavior)
  :custom ((org-hide-leading-stars t)
           (org-log-done 'time)
           (org-log-reschedule 'note)
           (org-log-redeadline 'note)
           (org-log-into-drawer "LOGBOOK")
           (org-return-follows-link t)
           (org-special-ctrl-a/e t)
           (org-treat-S-cursor-todo-selection-as-state-change nil)
           ;; Column view and estimates
           (org-columns-default-format "%80ITEM(Task) %7TODO(To Do) %10Effort(Estim){:} %10CLOCKSUM{+}")
           (org-global-properties '(("Effort_ALL" . "0:0 0:10 0:30 1:00 2:00 3:00 4:00 8:00")))
           (org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
           ;; Mark a task as DONE when archiving)
           (org-archive-mark-done nil)
           (org-src-fontify-natively t)
           (org-time-clocksum-use-effort-durations t)
           (org-M-RET-may-split-line '((default . nil)))
           (org-tags-column 0))
  :config
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-c C-x C-s" org-mode-map)
  (add-to-list 'org-modules 'org-habit))

(use-package org-agenda
  :after org
  :bind (("C-c a" . org-agenda))
  :config
  (require 'org-helper)
  (custom-set-variables
   '(org-agenda-start-on-weekday 6)
   '(org-agenda-span 'day)
   '(org-agenda-include-all-todo t)
   '(org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------"))
   '(org-agenda-log-mode-items '(clock))
   '(org-agenda-custom-commands '(("u" "Unscheduled" todo ""
                                   ((org-agenda-todo-ignore-scheduled t)))
                                  ("N" "Notes" tags "NOTE"
                                   ((org-agenda-overriding-header "Notes")
                                    (org-tags-match-list-sublevels t)))
                                  ("h" "Habits" tags-todo "STYLE=\"habit\""
                                   ((org-agenda-overriding-header "Habits")
                                    (org-agenda-sorting-strategy
                                     '(todo-state-down effort-up category-keep))))
                                  (" " "Agenda"
                                   ((agenda "" nil)
                                    (tags "REFILE"
                                          ((org-agenda-overriding-header "Tasks to Refile")
                                           (org-tags-match-list-sublevels nil)))
                                    (tags-todo "-CANCELLED/!"
                                               ((org-agenda-overriding-header "Stuck Projects")
                                                (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                                (org-agenda-sorting-strategy
                                                 '(priority-down category-keep))))
                                    (tags-todo "-HOLD-CANCELLED/!"
                                               ((org-agenda-overriding-header "Projects")
                                                (org-agenda-skip-function 'bh/skip-non-projects)
                                                (org-agenda-sorting-strategy
                                                 '(priority-down category-keep))))
                                    (tags-todo "-CANCELLED/!NEXT"
                                               ((org-agenda-overriding-header "Project Next Tasks")
                                                (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                                (org-tags-match-list-sublevels t)
                                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-sorting-strategy
                                                 '(priority-down todo-state-down effort-up category-keep))))
                                    (tags-todo "-REFILE-CANCELLED-WAITING/!"
                                               ((org-agenda-overriding-header (if (marker-buffer org-agenda-restrict-begin) "Project Subtasks" "Standalone Tasks"))
                                                (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                                                (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                                (org-agenda-sorting-strategy
                                                 '(category-keep)))
                                               (tags-todo "-CANCELLED+WAITING/!"
                                                          ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                                           (org-agenda-skip-function 'bh/skip-stuck-projects)
                                                           (org-tags-match-list-sublevels nil)
                                                           (org-agenda-todo-ignore-scheduled 'future)
                                                           (org-agenda-todo-ignore-deadlines 'future))))
                                    (tags "-REFILE/"
                                          ((org-agenda-overriding-header "Tasks to Archive")
                                           (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                                           (org-tags-match-list-sublevels nil))))
                                   nil)
                                  ("r" "Tasks to Refile" tags "REFILE"
                                   ((org-agenda-overriding-header "Tasks to Refile")
                                    (org-tags-match-list-sublevels nil)))
                                  ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
                                   ((org-agenda-overriding-header "Stuck Projects")
                                    (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                                  ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!NEXT"
                                   ((org-agenda-overriding-header "Next Tasks")
                                    (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                    (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                    (org-tags-match-list-sublevels t)
                                    (org-agenda-sorting-strategy
                                     '(todo-state-down effort-up category-keep))))
                                  ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                                   ((org-agenda-overriding-header "Tasks")
                                    (org-agenda-skip-function 'bh/skip-project-tasks-maybe)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                                  ("p" "Projects" tags-todo "-HOLD-CANCELLED/!"
                                   ((org-agenda-overriding-header "Projects")
                                    (org-agenda-skip-function 'bh/skip-non-projects)
                                    (org-agenda-sorting-strategy
                                     '(category-keep))))
                                  ("W" "Waiting Tasks" tags-todo "-CANCELLED+WAITING/!"
                                   ((org-agenda-overriding-header "Waiting and Postponed tasks"))
                                   (org-tags-match-list-sublevels nil))
                                  ("A" "Tasks to Archive" tags "-REFILE/"
                                   ((org-agenda-overriding-header "Tasks to Archive")
                                    (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                                    (org-tags-match-list-sublevels nil)))
                                  ("w" "Weekly review" agenda ""
                                   ((org-agenda-span 7) (org-agenda-log-mode 1)))))
   '(org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)
   '(org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 2))
   '(org-agenda-day-face-function #'jd:org-agenda-day-face-holidays-function)
   '(org-agenda-include-diary t))

  ;; Monkey patch agenda dimmed task function to skip tasks blocked by checkboxes
  (defadvice org-agenda-dim-blocked-tasks (around vh/org-agenda-dont-dim-checkbox-blocks activate)
    (let ((org-blocker-hook org-blocker-hook))
      (remove-hook 'org-blocker-hook 'org-block-todo-from-checkboxes)
      ad-do-it))

  ;; Merge gcal outputs to agenda views
  (require 'org-agenda-gcalcli)
  (add-hook 'org-agenda-finalize-hook 'vh/append-to-day-reports)

  ;; Search all my org files
  (let* ((all-files (apply 'append (mapcar (lambda (dir)
                                             (directory-files dir t ".*\\.org$"))
                                           (recursive-directory-list "~/org"))))
         (extra-files (delq nil (mapcar (lambda (extra-file)
                                          (unless (member extra-file org-agenda-files)
                                            extra-file))
                                        all-files))))
    (customize-set-variable 'org-agenda-text-search-extra-files extra-files)))

(use-package ox
  :after org
  :bind (:map org-mode-map
              ("C-c C-p" . org-publish-current-project))
  :config
  (custom-set-variables '(org-publish-use-timestamps-flag nil)
                        '(org-html-head-extra (concat "<style type=\"text/css\">"
                                                      "<!--/*--><![CDATA[/*><!--*/"
                                                      "pre.src {overflow-x: auto; }"
                                                      ".src { background-color: #f5deb3; color: #black;}"
                                                      "/*]]>*/-->"
                                                      "</style>"))))

(use-package ob-core :defer
  :after org
  :config
  (custom-set-variables '(org-babel-min-lines-for-block-output 999)
                        '(org-babel-results-keyword "results"))

  (org-babel-do-load-languages 'org-babel-load-languages '((ledger . t)
                                                           (dot . t)
                                                           (shell . t))))

(use-package org-capture
  :after org
  :custom ((org-default-notes-file "~/org/refile.org")
           (org-capture-templates '(("w" "Web" entry
                                     (file "~/org/inbox.org")
                                     "* %c :BOOKMARK:\n\n%i" :immediate-finish t
                                     :empty-lines 0)
                                    ("t" "TODO" entry
                                     (file+headline "~/org/inbox.org" "Incoming")
                                     "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :LINK: %a\n  :END:\n %i"
                                     :empty-lines-after 1)
                                    ("T" "TODO (clocked)" entry
                                     (file+headline "~/org/inbox.org" "Incoming")
                                     "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :LINK: %a\n  :END:\n %i"
                                     :clock-in t :clock-resume t
                                     :empty-lines-after 1)
                                    ("n" "note" entry
                                     (file "~/org/inbox.org")
                                     "* %? :NOTE:\n  %U\n  %a\n")
                                    ("N" "Note (clocked)" entry
                                     (file "~/org/inbox.org")
                                     "* %? :NOTE:\n  %U\n  %a\n"
                                     :clock-in t :clock-resume t)
                                    ("j" "journal" entry
                                     (file+olp+datetree "~/org/journal.org" "Daily Notes")
                                     "* %?\nEntered on %U\n  %i\n  %a"
                                     :empty-lines-after 1)
                                    ("q" "Quick note" item
                                     (file+headline "~/org/review.org" "Quick notes"))
                                    ("c" "Quick note on clocked task" item
                                     (clock)))))
  :bind (("C-c c" .  org-capture)))

(use-package org
  :config
  (custom-set-variables '(org-enforce-todo-checkbox-dependencies t)
                        '(org-enforce-todo-dependencies t)
                        '(org-use-fast-todo-selection t)
                        '(org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                                              (sequence "WAITING(w@/!)" "|" "HOLD(h@/!)" "CANCELLED(c@/!)" "PHONE" "MEETING")
                                              (sequence "QUOTE(q!)" "QUOTED(Q!)" "|" "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
                                              (sequence "OPEN(O)" "|" "CLOSED(C)")
                                              (type "PERIODIC(P)" "|" "DONE(d!/!)")))
                        '(org-todo-keyword-faces '(("TODO"      :foreground "red"          :weight bold)
                                                   ("PERIODIC"  :foreground "magenta"      :weight bold)
                                                   ("NEXT"      :foreground "blue"         :weight bold)
                                                   ("DONE"      :foreground "forest green" :weight bold)
                                                   ("WAITING"   :foreground "yellow"       :weight bold)
                                                   ("HOLD"      :foreground "goldenrod"    :weight bold)
                                                   ("CANCELLED" :foreground "orangered"    :weight bold)
                                                   ("PHONE"     :foreground "forest green" :weight bold)
                                                   ("MEETING"   :foreground "forest green" :weight bold)
                                                   ("QUOTE"     :foreground "hotpink"      :weight bold)
                                                   ("QUOTED"    :foreground "indianred1"   :weight bold)
                                                   ("APPROVED"  :foreground "forest green" :weight bold)
                                                   ("EXPIRED"   :foreground "olivedrab1"   :weight bold)
                                                   ("REJECTED"  :foreground "olivedrab"    :weight bold)
                                                   ("OPEN"      :foreground "magenta"      :weight bold)
                                                   ("CLOSED"    :foreground "forest green" :weight bold)))
                        '(org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
                                                         ("WAITING" ("WAITING" . t))
                                                         ("HOLD" ("WAITING" . t) ("HOLD" . t))
                                                         (done ("WAITING") ("HOLD"))
                                                         ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                                         ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                                         ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))


                        )

  )

(use-package org-habit :defer
  :after org-agenda
  :config
  (custom-set-variables '(org-habit-graph-column 80)
                        '(org-habit-show-habits-only-for-today nil)))

(use-package org
  :config
  (customize-set-value 'org-refile-use-outline-path 'file)
  ;; makes org-refile outline working with helm/ivy
  (customize-set-value 'org-outline-path-complete-in-steps nil)
  (customize-set-value 'org-refile-allow-creating-parent-nodes 'confirm))

(use-package org
  :hook ((org-mode . (lambda () (set-buffer-variable-pitch 'org-table 'org-code 'org-block 'org-meta-line)))))

(use-package org-superstar :ensure t
  :hook ((org-mode . org-superstar-mode))
  :config
  (custom-set-variables '(org-superstar-remove-leading-stars nil)
                        '(org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✚" "✜" "◆" "◇" "▶"))))

(use-package org-clock
  :after org
  :bind (("C-c C-x C-j" . org-clock-goto)
         ("C-c C-x TAB" . org-clock-in))
  :config
  (require 'org-helper)

  (custom-set-variables '(org-clock-persist t)
                        '(org-clock-history-length 28)
                        '(org-clock-in-resume t)
                        '(org-clock-into-drawer "CLOCK")
                        '(org-clock-out-remove-zero-time-clocks t)
                        '(org-clock-out-when-done t)
                        '(org-clock-persist t)
                        '(org-clock-auto-clock-resolution 'when-no-clock-is-running)
                        '(org-clock-report-include-clocking-task t)
                        '(org-clock-in-switch-to-state 'bh/clock-in-to-next)
                        '(org-clock-modeline-total 'current))
  (org-clock-persistence-insinuate)
  (org-clock-load))

(use-package diary-lib :defer
  :config
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t)
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files))

(use-package ledger-mode :ensure t
  :commands ledger-mode
  :custom ((ledger-add-transaction-prompt-for-text nil))
  :config
  (add-hook 'ledger-mode-hook (lambda ()
                                (setq ledger-post-account-alignment-column 2
                                      ledger-clear-whole-transactions t
                                      ledger-complete-ignore-case t
                                      ledger-highlight-xact-under-point nil)))

  (defadvice ledger-add-transaction (after remove-extra-newlines activate)
    "Clip the ever-growing \n series at end of file"
    (when (looking-at "\n\n\n")
      (delete-char 2))))

(use-package sdcv-mode :defer
  :bind ( ("C-c C-d" . sdcv-search)))

(use-package wgrep :ensure t)

(use-package dockerfile-mode :ensure t)

(use-package pdf-tools :ensure t :defer
  :config
  (custom-set-variables '(pdf-info-epdfinfo-program "/usr/bin/epdfinfo")))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode) :ensure t)

(use-package elfeed-org :ensure t
  :config
  (elfeed-org)
  (custom-set-variables '(rmh-elfeed-org-files '("~/org/feeds.org"))))

(defun elfeed-search-case-fold (orig-fun &rest args)
  (let ((case-fold-search t))
    (apply orig-fun args)))

(defun elfeed-search-compile-case-fold (orig-fun &rest args)
  (let ((filter (apply orig-fun args)))
    `(lambda (entry feed count) (let ((case-fold-search t))
                                  ,@(cddr filter)))))

(defun vh/elfeed-tag-untag-junk (arg)
  "Mark a post as junk.

When marking as junk, we also remove the unread tag. With prefix
argument, this function removes the junk tag (but doesn't add unread tag)."
  (interactive "p")
  (if (eq arg 4)
      (elfeed-search-untag-all 'junk)
    (let ((elfeed-search-remain-on-entry t))
      (elfeed-search-tag-all 'junk))
    (elfeed-search-untag-all 'unread)))

(use-package elfeed :ensure t
  :bind (:map elfeed-show-mode-map
              ("<tab>" . shr-next-link)
              :map elfeed-search-mode-map
              ("j" . vh/elfeed-tag-untag-junk))
  :hook (elfeed-show-mode . (lambda ()
                              (face-remap-add-relative 'variable-pitch :height browse-font-large-font-size)))

  :config
  (customize-set-value 'elfeed-search-filter "-junk @6-months-ago +unread")
  (advice-add #'elfeed-search-filter :around #'elfeed-search-case-fold)
  (advice-add #'elfeed-search-compile-filter :around #'elfeed-search-compile-case-fold))

(use-package hackernews :ensure t)

(use-package eww :ensure t
  :custom ((eww-search-prefix "https://duckduckgo.com/html?q=")
           (browse-url-browser-function #'eww)))

(use-package shr
  :config
  (setq shr-bullet "• "
        shr-use-fonts t
        shr-width 120))

(use-package url-cookie
  :config
  (custom-set-variables '(url-cookie-trusted-urls '())
                        '(url-cookie-untrusted-urls '(".*"))))

(let ((incubator-config-file (concat (expand-file-name "incubator-init.el" user-emacs-directory))))
  (when (file-exists-p incubator-config-file)
    (load-file incubator-config-file)))

(let ((local-config-file "~/.emacs-local-config.el"))
  (when (file-exists-p local-config-file)
    (load-file local-config-file)))
