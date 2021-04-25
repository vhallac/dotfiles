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

(ql:quickload :slynk)
(defcommand slynk () ()
  (slynk:create-server))

(set-prefix-key (kbd "s-t"))

(in-package :stumpwm)

(split-frame-in-dir (current-group) :column 500/809)
(add-group (current-screen) "Browse" :background t)
(add-group (current-screen) "Chat" :background t)
(add-group (current-screen) "Code" :background t)
(add-group (current-screen) "Temp1" :background t)
(add-group (current-screen) "Temp2" :background t)
(add-group (current-screen) "Temp3" :background t)
(add-group (current-screen) "Temp4" :background t)
(add-group (current-screen) "Temp5" :background t)
;;; This is my default for now
;;; Related to the Float-Master default, check out the fast group switch keybindings
(switch-to-group
 (add-group (current-screen) "Float-Dump" :background t :type 'float-group))
(add-group (current-screen) "Float-Master" :background t :type 'float-group)
(add-group (current-screen) "Float-Browse" :background t :type 'float-group)
(add-group (current-screen) "Float-Chat" :background t :type 'float-group)
(add-group (current-screen) "Float-Code" :background t :type 'float-group)
(add-group (current-screen) "Float-Temp1" :background t :type 'float-group)
(add-group (current-screen) "Float-Temp2" :background t :type 'float-group)
(add-group (current-screen) "Float-Temp3" :background t :type 'float-group)
(add-group (current-screen) "Float-Temp4" :background t :type 'float-group)
(add-group (current-screen) "Float-Temp5" :background t :type 'float-group)

(setf *mouse-focus-policy* :click)

(defvar *vh/fullscreen-rules* '( (:instance "emacs")
                                (:class "firefox")
                                (:instance "slack")
                                (:instance "xterm")
                                (:class "disable-jetbrains-idea" :type :NORMAL)))

(defun fullscreen-new-window-hook (new-window)
  (when (and (typep (current-group) 'float-group)
             (dolist (rule *vh/fullscreen-rules*)
               (when (apply #'window-matches-properties-p
                            (cons new-window rule))
                 (return rule))))
    (setf (window-fullscreen new-window) t)))


(add-hook *new-window-hook* #'fullscreen-new-window-hook)

(define-key *root-map* (kbd "c") "exec /usr/bin/uxterm")
(define-key *root-map* (kbd "C-c") "exec /usr/bin/uxterm")

;; TODO: Convert to loop, add all modifier combinations:
;; S, C, M, and all their permutations
(define-key *top-map* (kbd "s-h") "meta Left")
(define-key *top-map* (kbd "s-j") "meta Down")
(define-key *top-map* (kbd "s-k") "meta Up")
(define-key *top-map* (kbd "s-l") "meta Right")

(define-key *top-map* (kbd "s-C-h") "meta C-Left")
(define-key *top-map* (kbd "s-C-j") "meta C-Down")
(define-key *top-map* (kbd "s-C-k") "meta C-Up")
(define-key *top-map* (kbd "s-C-l") "meta C-Right")

(define-key *top-map* (kbd "s-H") "meta S-Left")
(define-key *top-map* (kbd "s-J") "meta S-Down")
(define-key *top-map* (kbd "s-K") "meta S-Up")
(define-key *top-map* (kbd "s-L") "meta S-Right")

(define-key *top-map* (kbd "s-C-H") "meta C-S-Left")
(define-key *top-map* (kbd "s-C-J") "meta C-S-Down")
(define-key *top-map* (kbd "s-C-K") "meta C-S-Up")
(define-key *top-map* (kbd "s-C-L") "meta C-S-Right")

(define-key *top-map* (kbd "s-G") "gselect")

(defvar *vh/active-group-type* :float)

(defun seq (end &key (start 1))
  "Generate integer sequence ending at END, and optionally starting at START."
  (loop for i from start to end collect i))

(defun switch-to-tiled ()
  "Convert the fast group switch map to tiled groups"
  (setf *vh/active-group-type* :tiled)
  (dolist (i (seq 9))
    (define-key *top-map* (kbd (format nil "s-~a" i)) (format nil "gselect ~a" i))
    (define-key *top-map* (kbd (format nil "s-C-~a" i)) (format nil "gmove ~a" i))))

(defun switch-to-float ()
  "Convert the fast group switch map to tiled groups"
  (setf *vh/active-group-type* :float)
  (dolist (i (seq 9))
    (define-key *top-map* (kbd (format nil "s-~d" i)) (format nil "gselect ~d" (+ 10 i)))
    (define-key *top-map* (kbd (format nil "s-C-~d" i)) (format nil "gmove ~d" (+ 10 i)))))

(defcommand toggle-fast-switch-set () ()
  "Changes group switch keybindings to either the tiled or the float set."
  (if (eql *vh/active-group-type* :tiled)
      (switch-to-float)
      (switch-to-tiled)))

(switch-to-float)

(define-key *groups-map* (kbd "t") "toggle-fast-switch-set")

(define-key *root-map* (kbd "s-g") "gother")

(define-key *top-map* (kbd "s-D") "exec rofi -show-icons -show drun")
(define-key *top-map* (kbd "s-d") "exec rofi -show-icons -show run")
(define-key *top-map* (kbd "s-W") "exec rofi -show-icons -show window")
(define-key *top-map* (kbd "s-w") "exec rofi -show-icons -show windowcd")
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
(define-key *vh/user-map* (kbd "w") "exec xsel -b | xargs mpv")
(define-key *vh/user-map* (kbd "l") "exec xsel -b | xargs mpv --vo=null")

(defvar *vh/timer-map* (make-sparse-keymap)
  "Keymap for time logging and timers")

(define-key *vh/user-map* (kbd "t") *vh/timer-map*)

(define-key *vh/timer-map* (kbd "t") "exec emacsclient -n -e '(pia/org-clock-in-team)'")
(define-key *vh/timer-map* (kbd "u") "exec emacsclient -n -e '(pia/org-clock-in-unexpected)'")
(define-key *vh/timer-map* (kbd "g") "exec emacsclient -c -n -e '(org-clock-goto)'")
(define-key *vh/timer-map* (kbd "l") "exec emacsclient -n -e '(my/org-clock-in-interrupted)'")
(define-key *vh/timer-map* (kbd "o") "exec emacsclient -n -e '(org-clock-out)'")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec light -A 5")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec light -U 5")
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl set-sink-mute 1 toggle")
(define-key *top-map* (kbd "XF86AudioMicMute") "exec pactl set-source-mute 2 toggle")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec pactl set-sink-volume 1 +5%")
(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec pactl set-sink-volume 1 -5%")

(define-key *top-map* (kbd "s-i") "time")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")

(define-key *tile-group-root-map* (kbd "s-s") "vsplit 2/3")
(define-key *tile-group-root-map* (kbd "s-S") "hsplit 2/3")

(require 'sb-posix)

(sb-posix:setenv "GDK_CORE_DEVICE_EVENTS" "1" 1)

(sb-posix:setenv "QT_AUTO_SCREEN_SCALE_FACTOR" "0" 1)

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

(flet ((add-float-window (group window raise)
         (let ((fullscreen-p (window-fullscreen window)))
           (change-class window 'float-window)
           (float-window-align window)
           (when fullscreen-p
             (activate-fullscreen window))
           (when raise
             (group-focus-window group window)))))
  (defmethod group-add-window ((group float-group) window &key raise &allow-other-keys)
    (add-float-window group window raise))
  (defmethod group-add-window (group (window float-window) &key raise &allow-other-keys)
    (add-float-window group window raise)))
