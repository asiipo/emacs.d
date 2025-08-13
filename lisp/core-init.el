;;; core-init.el --- Backups, autosaves, editing defaults -*- lexical-binding: t; -*-

;; Backups and auto-saves to ~/.emacs.d/var/
(let* ((var-dir (expand-file-name "var/" user-emacs-directory))
       (backup-dir (expand-file-name "backups/" var-dir))
       (autosave-dir (expand-file-name "auto-saves/" var-dir)))
  (dolist (d (list backup-dir autosave-dir)) (make-directory d t))
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,autosave-dir t))
        auto-save-default t
        create-lockfiles nil
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        version-control t))

;; Sensible editing defaults
(setq sentence-end-double-space nil
      require-final-newline t
      confirm-kill-emacs nil
      confirm-kill-processes nil)

(provide 'core-init)

