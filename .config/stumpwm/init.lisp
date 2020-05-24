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

(load "~/.sbclrc")

(ql:quickload :swank)
(defcommand swank () ()
            (swank:create-server
             :dont-close t
             :port swank::default-server-port))

(set-prefix-key (kbd "s-t"))

(in-package :stumpwm)

(split-frame-in-dir (current-group) :column 500/809)
(add-group (current-screen) "Browse" :background t)
(add-group (current-screen) "Code" :background t)
(add-group (current-screen) "Chat" :background t)
(add-group (current-screen) "Temp1" :background t)
(add-group (current-screen) "Temp2" :background t)
(add-group (current-screen) "Temp3" :background t)
(add-group (current-screen) "Float-1" :background t :type 'float-group)
(add-group (current-screen) "Float-2" :background t :type 'float-group)

(setf *mouse-focus-policy* :click)

(defun osd-new-window-hook (new-window)
  (when (window-matches-properties-p new-window :instance "pqiv")
    (push-event (lambda ()
                  (if (not (typep (current-group) 'float-group))
                      (float-window new-window (current-group)))
                  (push-event (lambda ()
                                (push new-window (group-on-top-windows (current-group)))
                                (setf (window-fullscreen new-window) t)))))))

(defun osd-focus-window-hook (new-window old-window)
  (when (window-matches-properties-p new-window :instance "pqiv")
    (push-event (lambda ()
                  (focus-window old-window)))))

(add-hook *new-window-hook* #'osd-new-window-hook)

(add-hook *focus-window-hook* #'osd-focus-window-hook)

(define-key *root-map* (kbd "c") "exec /usr/bin/uxterm")
(define-key *root-map* (kbd "C-c") "exec /usr/bin/uxterm")

(define-key *top-map* (kbd "s-G") "gselect")

(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-6") "gselect 6")
(define-key *top-map* (kbd "s-7") "gselect 7")
(define-key *top-map* (kbd "s-8") "gselect 8")
(define-key *top-map* (kbd "s-9") "gselect 9")

(define-key *top-map* (kbd "s-C-0") "gmove 10")
(define-key *top-map* (kbd "s-C-1") "gmove 1")
(define-key *top-map* (kbd "s-C-2") "gmove 2")
(define-key *top-map* (kbd "s-C-3") "gmove 3")
(define-key *top-map* (kbd "s-C-4") "gmove 4")
(define-key *top-map* (kbd "s-C-5") "gmove 5")
(define-key *top-map* (kbd "s-C-6") "gmove 6")
(define-key *top-map* (kbd "s-C-7") "gmove 7")
(define-key *top-map* (kbd "s-C-8") "gmove 8")
(define-key *top-map* (kbd "s-C-9") "gmove 9")
(define-key *top-map* (kbd "s-C-0") "gmove 10")

(define-key *top-map* (kbd "s-D") "exec dmenu_run")
(define-key *top-map* (kbd "s-d") "exec dmenu_run_history")
(define-key *top-map* (kbd "s-F") "fullscreen")
(define-key *top-map* (kbd "s-Q") "delete-window")
(define-key *top-map* (kbd "s-C-Q") "kill-window")

(defvar *vh/session-map* (make-sparse-keymap)
  "Keymap for session and power management functions")

(define-key *top-map* (kbd "s-p") *vh/session-map*)

(define-key *vh/session-map* (kbd "s-l") "exec loginctl lock-session")
(define-key *vh/session-map* (kbd "s-o") "exec loginctl poweroff")
(define-key *vh/session-map* (kbd "s-r") "exec loginctl reboot")
(define-key *vh/session-map* (kbd "s-s") "exec loginctl suspend")

(defvar *vh/user-map* (make-sparse-keymap)
  "Keymap for odds and ends")

(define-key *top-map* (kbd "s-c") *vh/user-map*)

(define-key *vh/user-map* (kbd "s-P") "exec /usr/bin/keepass --auto-type")

(define-key *vh/user-map* (kbd "c") "exec xsel | xsel -i -b")
(define-key *vh/user-map* (kbd "C") "exec xsel -b | xsel -i")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec light -A 5")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec light -U 5")
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl set-sink-mute 1 toggle")
(define-key *top-map* (kbd "XF86AudioMicMute") "exec pactl set-source-mute 2 toggle")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume 1 +5%")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume 1 -5%")

(define-key *top-map* (kbd "s-i") "time")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")

(require 'sb-posix)

(sb-posix:setenv "GDK_CORE_DEVICE_EVENTS" "1" 1)

(run-prog "/usr/bin/lxsession" :args (split-string "-e stumpwm -s stumpwm" " ") :wait nil)

(defun new-window-adjustments (new-window)
  (when (or (window-matches-properties-p new-window :instance "gitk")
            (window-matches-properties-p new-window :instance "git-gui"))
    (setf (window-fullscreen new-window) t)))

(add-hook *new-window-hook* #'new-window-adjustments)

(defun float-dialogs-hook (win)
  (let ((grp (current-group)) )
    (when (and (not (typep grp 'float-group))
               (or
                (eq (window-type win) :dialog)
                (string-match (window-res win) "pinentry-gtk-2")))
      (float-window win grp)
      (focus-window win t))))

(add-hook *new-window-hook* #'float-dialogs-hook)

(redirect-all-output "~/.cache/stumpwm/log")
