;;; bp.el --- Background process managment

;; Author: Ilya Babanov <ilya-babanov@ya.ru>
;; Version: 1.0.0
;; Keywords: background, process, managment
;; URL: https://github.com/ilya-babanov/emacs-bp

;;; Commentary:
;; This package provides functionality for running processes in background

(defcustom bp-close-after-success nil
  "Should process's window be closed after success"
  :type 'boolean)

(defcustom bp-open-after-error t
  "Should window with process output be opened after error"
  :type 'boolean)

(defcustom bp-window-creator #'split-window-vertically
  "Function for creating window for process"
  :type 'function)

(defcustom bp-process-mode #'shell-mode
  "Mode for process's buffer"
  :type 'function)

(defcustom bp-process-directory nil
  "Directory for process.
If not nil, it will be assigned to default-direcotry.
If nil, standart default-direcotry will be used,
or projectile-project-root, if it's available."
  :type 'string)

(defcustom bp-erase-process-buffer t
  "Shuld process's buffer be erased before starting new process"
  :type 'boolean)

(defcustom bp-scroll-direction 1
  "Scroll text in error window, -1 for scroll up, 1 - scroll down"
  :type 'number)

(defcustom bp-show-progress t
  "Should process's progress be shown"
  :type 'boolean)

(defcustom bp-poll-timout 0.2
  "Progress update interval"
  :type 'number)

(defun bp-spawn (cmd)
  "Invokes passed command in background"
  (interactive "sCommand:")
  (let* ((proc-name (bp-create-process-name cmd))
         (process (get-process proc-name)))
    (if process
        (progn
          (message "Process already exist: %s" process)
          (bp-try-refresh-process-window process))
      (bp-run-process cmd))))

(defun bp-run-process (cmd)
  (message "Running process '%s'" cmd)
  (let* ((default-directory (bp-get-current-directory))
         (proc-name (bp-create-process-name cmd))
         (buff-name (concat "*" proc-name "*"))
         (buffer (get-buffer-create buff-name))
         (process (start-process-shell-command proc-name buffer cmd)))
    (message default-directory)
    (set-process-plist process (bp-create-process-plist))
    (set-process-sentinel process 'bp-handle-result)
    (bp-handle-progress process)
    (bp-config-process-buffer buffer)))

(defun bp-get-current-directory ()
  (if bp-process-directory
      bp-process-directory
    (bp-try-get-project-root)))

(defun bp-try-get-project-root ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    default-directory))

(defun bp-create-process-name (cmd)
  (concat cmd " (" (bp-get-current-directory) ")"))

(defun bp-create-process-plist ()
  (list 'poll-timeout bp-poll-timout
        'close-after-success bp-close-after-success
        'open-after-error bp-open-after-error
        'show-progress bp-show-progress
        'window-creator bp-window-creator
        'scroll-direction bp-scroll-direction
        'start-time (float-time)))

(defun bp-config-process-buffer (buffer)
  (with-current-buffer buffer
    (if bp-erase-process-buffer (erase-buffer))
    (funcall bp-process-mode)))

(defun bp-handle-progress (process)
  (if (process-live-p process)
      (let* ((show-progress (process-get process 'show-progress)))
        (when show-progress (bp-show-progress-message process))
        (bp-delay-progress-handler process))))

(defun bp-delay-progress-handler (process)
  (let* ((poll-timeout (process-get process 'poll-timeout)))
    (run-at-time poll-timeout nil 'bp-handle-progress process)))

(defun bp-handle-result (process &optional event)
  (bp-colorize-process-buffer process)
  (unless (process-live-p process)
    (let* ((exit-code (process-exit-status process)))
      (if (= exit-code 0)
          (bp-handle-success process)
        (bp-handle-error process exit-code)))))

(defun bp-handle-success (process)
  (bp-show-success-message process)
  (let* ((buffer-window (bp-get-process-window process))
         (close-after-success (process-get process 'close-after-success)))
    (when (and buffer-window close-after-success)
      (delete-window buffer-window))))

(defun bp-handle-error (process exit-code)
  (bp-show-error-message process exit-code)
  (let* ((buffer (process-buffer process))
         (buffer-window (get-buffer-window buffer))
         (open-after-error (process-get process 'open-after-error)))
    (when (and open-after-error (not buffer-window))
      (setq buffer-window (funcall (process-get process 'window-creator)))
      (set-window-buffer buffer-window buffer))
    (bp-try-refresh-process-window process)))

(defun bp-show-progress-message (process)
  (let* ((status (process-status process))
         (time-diff (bp-get-process-time-diff process)))
    (message "Status: %s   Time: %.1f   Process: %s" status time-diff process)))

(defun bp-show-success-message (process)
  (message "Status: %s   Time: %.3f   Process: %s"
           (propertize "Success" 'face '(:foreground "green"))
           (bp-get-process-time-diff process)
           process))

(defun bp-show-error-message (process exit-code)
  (message "Status: %s   Code: %s   Time: %.3f   Process: %s"
           (propertize "Error" 'face '(:foreground "red"))
           exit-code
           (bp-get-process-time-diff process)
           process))

(defun bp-get-process-time-diff (process)
  (let* ((start-time (process-get process 'start-time)))
    (- (float-time) start-time)))

(defun bp-get-process-window (process)
  (let* ((buffer (process-buffer process)))
    (get-buffer-window buffer)))

(defun bp-try-refresh-process-window (process)
  (let* ((window (bp-get-process-window process))
         (scroll-direciton (process-get process 'scroll-direction)))
    (when window (bp-refresh-process-window window scroll-direciton))))

(defun bp-refresh-process-window (window direction)
  (with-selected-window window
    (ignore-errors
      (scroll-down-command (bp-get-remaining-lines-count direction)))))

(defun bp-colorize-process-buffer (process)
  (with-current-buffer (process-buffer process)
      (ansi-color-apply-on-region (point-min) (point-max))))

(defun bp-get-remaining-lines-count (direction)
    (count-lines (point) (buffer-end direction)))

(provide 'bp)
