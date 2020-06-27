;;; persistent-echo.el -- persistent echo messages -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; Package to keep displaying messages in the echo area whenever it's free.
;; Taken from [[https://github.com/ThibautVerron/echo-sth.el]]
;;
;;; Code:

(defcustom persistent-echo-refresh-delay 1
  "Refresh rate for the empty echo area message")

(defcustom persistent-echo-delay 1
  "How long to wait after the echo area is empty to start using it")
(defun persistent-echo--default-function ()
  "Default echo function which prints an error."
  "Please set `persistent-echo-display-function' to a useful value!")

(defcustom persistent-echo-display-function #'persistent-echo--default-function
  "Function generating the string to display in the echo area")


(defvar persistent-echo--display-active nil)
(defvar persistent-echo--timer-objects nil)

(defun persistent-echo--display ()
  "Activate empty echo area display."
  (interactive)
  (unless (or (active-minibuffer-window) (current-message))
    (let* ((message-log-max nil)) ; do not insert to *Messages* buffer
      (message "%s" (funcall persistent-echo-display-function)))
    (setq persistent-echo--display-active t)))

(defun persistent-echo--redisplay ()
  "Update empty echo area display."
  (when persistent-echo--display-active (persistent-echo--display)))

(defun persistent-echo--display-end ()
  "Deactivate empty echo area display."
  (setq persistent-echo--display-active nil))

;;;###autoload
(define-minor-mode persistent-echo-mode
  "Use the echo area to display something if it is empty"
  :init-value nil
  :global t
  (cond (persistent-echo-mode (and persistent-echo-display-function
                            (setq persistent-echo--timer-objects
                                  (list (run-with-timer
                                         0 persistent-echo-refresh-delay
                                         #'persistent-echo--redisplay)
                                        (run-with-idle-timer
                                         persistent-echo-delay t
                                         #'persistent-echo--display)))
                            (add-hook 'pre-command-hook
                                      #'persistent-echo--display-end)))
        (t (remove-hook 'pre-command-hook #'persistent-echo--display-end)
           (mapc 'cancel-timer persistent-echo--timer-objects))))


;;;###autoload
(defun persistent-echo-enable (display-function)
  "Enable `persistent-echo-mode' with DISPLAY-FUNCTION providing the messages."
  (persistent-echo-mode -1)
  (setq persistent-echo-display-function display-function)
  (persistent-echo-mode))


(provide 'persistent-echo)
;;; persistent-echo.el ends here
