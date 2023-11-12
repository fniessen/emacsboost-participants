;;; init_emacsboost --- XXX

;;; Commentary:

;;; Code:

;; 7.4 Bind F11 to the undo command.
(global-set-key (kbd "<f11>") 'undo)

;; 12.1 Define a function to duplicate the current line or region.
(defun lvn-duplicate-line-or-region ()
  "Duplicate the current line or the region if active."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (kill-ring-save (region-beginning) (region-end))
      (progn
        (kill-ring-save (line-beginning-position) (line-end-position))
        (end-of-line)))
    ;; (newline)
    ;; (yank)
    (indent-according-to-mode)))

;; Bind C-S-d to the duplication function.
(global-set-key (kbd "C-S-d") 'lvn-duplicate-line-or-region)

;; 12.3
(setq interprogram-paste-function
      (if (equal interprogram-paste-function 'x-cut-buffer-or-selection-value)
          'x-selection-value
        interprogram-paste-function))

;; 14.13 Indicate changes in the fringe.
(with-eval-after-load "diff-hl"
  (global-diff-hl-mode 1))

;; 14.15 Check if the fill-column-indicator package is available.
(if (require 'fill-column-indicator nil 'noerror)
    (progn
      (setq fci-rule-column 80) ; Set the desired column
      (add-hook 'prog-mode-hook 'fci-mode)
      (add-hook 'text-mode-hook 'fci-mode))
  (message "Warning: 'fill-column-indicator' package is not installed. Line indicator will not be shown."))

;; 14.20 Enable highlighting of the current line.
(global-hl-line-mode 1)

;; 18.2 Bind F3 to the find-file command.
(global-set-key (kbd "<f3>") 'find-file)

;; 18.3 Bind F2 to save the buffer.
(global-set-key (kbd "<f2>") 'save-buffer)

;; 18.3 Enable numbered backups.
(setq version-control t)

;; Set the backup directory path for Emacs backups.
(setq lvn-backup-directory "~/.emacs.d/backups/")

;; Create the backup directory if it doesn't exist.
(when (not (file-exists-p lvn-backup-directory))
  (make-directory lvn-backup-directory t))

;; Configure backup files to be saved in the central backup location.
(setq backup-directory-alist `(("." . ,lvn-backup-directory)))

;; 18.4 Bind ... to revert-buffer.
(defun lvn-revert-buffer ()
  "Revert the current buffer unconditionally and remove specified highlights."
  (interactive)
  (revert-buffer t t) ; ignore-auto(-save), noconfirm
  (message "[Buffer is up to date with the file on disk]"))
(global-set-key (kbd "C-S-z") 'lvn-revert-buffer)
(global-set-key (kbd "C-S-y") 'lvn-revert-buffer)

;; 18.4 Kill the current buffer without confirmation (if not modified).
(defun lvn-kill-current-buffer-no-confirm ()
  "Kill the current buffer without confirmation (if not modified)."
  (interactive)
  (kill-buffer nil))

;; Key binding for killing the current buffer.
(global-set-key (kbd "<S-f12>") 'lvn-kill-current-buffer-no-confirm)

;; 20.3 Switch to the previous buffer or rotate window configuration.
(defun lvn-rotate-or-previous-buffer ()
  "Switch to the previous buffer or rotate window configuration.

If there is only one window in the frame, this function switches
to the previous buffer, cycling through the buffer list in the
current window.

If there are multiple windows in the frame, this function rotates
the window configuration, moving to the previous window in the
cyclic order."
  (interactive)
  (if (one-window-p t)
      (switch-to-buffer (other-buffer (current-buffer) 1))
    (other-window -1)))

(global-set-key (kbd "<f6>") 'lvn-rotate-or-previous-buffer)

;; 20.5 Toggle or delete the window layout.
(defun lvn-toggle-or-delete-window-layout ()
  "Toggle or delete the window layout.

If there is only one window in the frame, this function will split the window
either horizontally or vertically, depending on the frame's width, as defined by
`split-width-threshold' variable. If the frame width is greater than
`split-width-threshold', it will split the window horizontally, otherwise
vertically.

If there are multiple windows in the frame, this function will delete all other
windows, leaving only the currently active window visible."
  (interactive)
  (cond ((one-window-p t)
         (select-window
          (if (> (frame-width) split-width-threshold)
              (split-window-horizontally)
            (split-window-vertically))))
        (t
         (delete-other-windows))))

(global-set-key (kbd "<f5>") 'lvn-toggle-or-delete-window-layout)

;; 24.3 Set the default for indentation to use spaces instead of tabs.
(setq-default indent-tabs-mode nil)

;; 25.5 Line-wrapping beyond that column (when pressing `M-q').
(setq-default fill-column 80)

;; 25.9 Add an auto-mode entry for .txt files to use org-mode.
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; org5 Customize org-mode settings.
(setq org-log-states-order-reversed nil)

;; org11 Hide emphasis markers in org mode.
(setq org-hide-emphasis-markers t)

;; org15
(setq org-confirm-babel-evaluate nil)

;; 28.1 Follow symbolic links.
(setq vc-follow-symlinks t)

;; 28.1.9 Jump to the VC status buffer for the current directory.
(defun lvn-jump-to-vc-status-buffer-for-current-directory ()
  "Jump to the VC status buffer for the current directory."
  (interactive)
  (let ((directory (or (file-name-directory (buffer-file-name))
                       default-directory)))
    (message "[VC status for directory: %s]" directory)
    (vc-dir directory)))

;; VC status without asking for a directory.
(global-set-key (kbd "<C-f9>")
                'lvn-jump-to-vc-status-buffer-for-current-directory)

;; Setup keybindings and behavior for VC directory mode.
(defun lvn-vc-dir-setup ()
  "Setup keybindings and behavior for VC directory mode."
  (define-key vc-dir-mode-map (kbd "x")
              'lvn-hide-up-to-date-and-unregistered-files-in-vc-dir)
  (define-key vc-dir-mode-map (kbd "E")
              'vc-ediff))

(add-hook 'vc-dir-mode-hook 'lvn-vc-dir-setup)

;; Hide up-to-date and unregistered files in VC directory buffer.
(defun lvn-hide-up-to-date-and-unregistered-files-in-vc-dir ()
  "Hide up-to-date and unregistered files in VC directory buffer."
  (interactive)
  (vc-dir-hide-up-to-date)
  (lvn-vc-dir-hide-unregistered))

;; Hide unregistered items from display in VC directory buffer.
(defun lvn-vc-dir-hide-unregistered ()
  "Hide 'unregistered' items from display in VC directory buffer."
  (interactive)
  (let ((current-item (ewoc-nth vc-ewoc -1))
        (first-item (ewoc-nth vc-ewoc 0)))
    ;; Iterate from the last item to the first and remove unregistered files
    ;; and directories without child files.
    (while (not (eq current-item first-item))
      (let* ((item-data (ewoc-data current-item))
             (is-directory (vc-dir-fileinfo->directory item-data))
             (next-item (ewoc-next vc-ewoc current-item))
             (prev-item (ewoc-prev vc-ewoc current-item))
             ;; Necessary for ewoc-delete to work...
             (inhibit-read-only t))
        (when (or
               ;; Remove directories with no child files.
               (and is-directory
                    (or
                     ;; No item follows this directory.
                     (not next-item)
                     ;; Next item is a directory.
                     (vc-dir-fileinfo->directory (ewoc-data next-item))))
               ;; Remove files in the unregistered state.
               (eq (vc-dir-fileinfo->state item-data) 'unregistered))
          (ewoc-delete vc-ewoc current-item))
        (setq current-item prev-item)))))

;; 39 Use Emacs as a server (with the `emacsclient' program).
(unless noninteractive
  (require 'server))             ; After init.

;; Start the Emacs server if it's not already running.
(with-eval-after-load "server"
  (unless (equal (server-running-p) t)
    (server-start))

  ;; Save file without confirmation before returning to the client.
  (defadvice server-edit (before save-buffer-if-needed activate)
    "Save current buffer before marking it as done."
    (when server-buffer-clients
      (save-buffer))))



(defconst lvn--wsl-p
  (let ((kernel-release (string-trim (shell-command-to-string "uname -r"))))
    (or (string-match "WSL" kernel-release)
        (string-match "microsoft-standard-WSL2" kernel-release)))
  "Running Emacs on WSL or WSL2.")



;; Ajouter tous les répertoires des paquets MELPA au chemin de chargement.
(let ((package-dirs (directory-files
                     package-user-dir
                     t "^[^.].*")))
  (while package-dirs
    (add-to-list 'load-path (car package-dirs))
    (setq package-dirs (cdr package-dirs))))



(with-eval-after-load "projectile"
  ;; Turn on projectile mode by default for all file types
  (projectile-mode)

  ;; Add keymap prefix.
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))



(provide 'emacsboost)

;;; init_emacsboost.el ends here
