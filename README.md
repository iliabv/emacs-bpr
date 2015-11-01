# Emacs-BP
This package provides handy logic for managing processes in background.

`bp` can:
- spawn async processes in background (you are free to do other things, while process is being executed)
- show progress messages in echo area
- open windows with output buffers in case of errors

`bp` is best for running tests or build for your projects, but you can run any processes with it. 

# Example
Given this configuration:
```elisp
(require 'bp)

;; define function for running desired process
(defun grunt-tests ()
  "Spawns 'grunt test' task"
  (interactive)
  (let* ((bp-scroll-direction -1)
         (bp-close-after-success t))
    (bp-spawn "grunt test --color")))

;; set key-binding
(define-key global-map "\C-ct" 'grunt-tests)
```
You get this:
![grunt test example](./img/run-grunt-test.gif)

* What's happening:
- User enters predefined key-binding, which invokes function `grunt-tests`.
- `bp-spawn` starts async process `grunt test --color` and writes progress messages in echo area.
- If process ends successfully - success message is being shown.
- If process ends with error - error message is being shown and window with output buffer is being opened.

# Installation
### MELPA:
Use `M-x package-install bp` and write `(require 'bp)` in your conifg.

### Manually
```elisp
;; If you have cloned this repo into `~/some-path/emacs-bp/`
(add-to-list 'load-path "~/some-path/emacs-bp/")
(require 'bp)
```

# Configuration
You can find all configuration options in the source code.

Default directory for processes is `default-directory` of current buffer, but with `projectile` installed, `bp` would use `projectile-project-root` function.

Default major mode for process's output buffer is `shell-mode`. Note, that this buffer is only showed in case of error, but you can manually open it at any time. Template for buffers names: `*process-name (process-directory)*`

### Examples for different use cases
##### Running tests
```elisp
(defun my-test-runner ()
  "Spawns test process"
  (interactive)
  (let* ((bp-scroll-direction -1) ;; scroll to the top of the output window (wich is being shown in case of error)
         (bp-close-after-success t)) ;; close error window after process ended successfully (if it's not already closed)
    (bp-spawn "rake tests")))
```
##### Running builds
```elisp
(defun my-test-runner ()
  "Spawns test process"
  (interactive)
  (let* ((bp-process-directory "~/chromium/") ;; spawn process in this directory (instead of default-directory or projectile-project-root)
         (bp-poll-timout 60.0)) ;; show progress messages once in 60 seconds
    (bp-spawn "make long-build")))
```

