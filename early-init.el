;;; early-init.el --- Startup/UI polish -*- lexical-binding: t; -*-

;; GC and IO
(setq gc-cons-threshold (* 64 1024 1024)
      read-process-output-max (* 4 1024 1024)
      load-prefer-newer t)

;; Package init handled later
(setq package-enable-at-startup nil)

;; UI minimalism early
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil)
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; Start with a larger frame (in character columns/rows)
(add-to-list 'default-frame-alist '(width . 160))
(add-to-list 'default-frame-alist '(height . 60))

;;; early-init.el ends here

