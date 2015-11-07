;;; test-bpr.el --- Tests for Background Process Runner

;;; Commentary:
;; WIP

;;; Code:
(require 'bpr)
(require 'buttercup)

(describe "defaults"
  (it "should have correct values"
    (expect bpr-close-after-success :to-be nil)
    (expect bpr-open-after-error :to-be t)
    (expect bpr-window-creator :to-be #'split-window-vertically)
    (expect bpr-process-mode :to-be #'shell-mode)
    (expect bpr-process-directory :to-be nil)
    (expect bpr-use-projectile :to-be t)
    (expect bpr-erase-process-buffer :to-be t)
    (expect bpr-scroll-direction :to-be 1)
    (expect bpr-show-progress :to-be t)
    (expect bpr-poll-timout :to-equal 0.2)
    (expect bpr-open-after-error :to-be t)))

(describe "bpr-spawn"
  (let* (message
         get-buffer-create
         erase-buffer
         shell-mode
         process-live-p
         set-process-plist
         set-process-sentinel
         start-process-shell-command)

    (before-each
      (fset 'message (lambda (str &rest args) nil))
      (fset 'get-buffer-create (lambda (name) nil))
      (fset 'erase-buffer (lambda (buffer) nil))
      (fset 'shell-mode (lambda () nil))
      (fset 'process-live-p (lambda (process) nil))
      (fset 'set-process-plist (lambda (process list) nil))
      (fset 'set-process-sentinel (lambda (process func) nil))
      (fset 'start-process-shell-command (lambda (name buffer command) nil))

      (setq default-directory "/test/")

      (spy-on 'get-buffer-create)
      (spy-on 'start-process-shell-command))

    (it "should set correct name for process buffer"
      (bpr-spawn "ls")
      (expect 'get-buffer-create
              :to-have-been-called-with
              (concat "*ls (" default-directory ")*")))

    ;; All directory checks are made by checking process buffer name...
    ;; This is because I haven't found direct way to do it.
    (it "should set correct directory for process with default options"
      (let* ((default-directory "./test/test/test/"))
        (bpr-spawn "ls")
        (expect 'get-buffer-create
                :to-have-been-called-with
                "*ls (./test/test/test/)*")))

    (it "should set correct directory for process using projectile"
      (flet ((projectile-project-root () "/projects/root/"))
        (bpr-spawn "ls")
        (expect 'get-buffer-create
                :to-have-been-called-with
                "*ls (/projects/root/)*")))

    (it "should not use projectile when bpr-use-projectile is nil"
      (flet ((projectile-project-root () "/projects/root/"))
        (let ((default-directory ".")
              (bpr-use-projectile nil))
          (bpr-spawn "ls")
          (expect 'get-buffer-create
                  :to-have-been-called-with
                  "*ls (.)*"))))

    (it "should use bpr-process-directory if it's not nil"
      (flet ((projectile-project-root () "/projects/root/"))
        (let ((default-directory ".")
              (bpr-process-directory "should/use/this"))
          (bpr-spawn "ls")
          (expect 'get-buffer-create
                  :to-have-been-called-with
                  "*ls (should/use/this)*"))))

    (it "should spawn process with correct name, buffer and command"
      (let* ((default-directory "dev/null")
             (fake-buffer (generate-new-buffer "test buffer")))
        (fset 'get-buffer-create (lambda (name) fake-buffer))
        (bpr-spawn "ls")
        (expect 'start-process-shell-command
                :to-have-been-called-with
                "ls (dev/null)" fake-buffer "ls")))

    ))

;;; test-bpr.el ends here
