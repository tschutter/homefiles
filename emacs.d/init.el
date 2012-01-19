;;;; Local config.
(if (file-exists-p "~/.emacs-local")
    (load-file "~/.emacs-local"))


;;;; ~/.emacs.d/lisp/
;;; Some packages installed in ~/.emacs.d/lisp/ are single files while others
;;; are placed inside their own sub-directories.
;;; Prepend ~/.emacs.d/lisp/ and all of it's subdirectories to load-path.
;;; See http://www.emacswiki.org/emacs/LoadPath#AddSubDirectories
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))


;;;; Desktop
(desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'query-replace-history)


;;;; Web browsing
(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;;; Save the w3m buffers between sessions with desktop.el.
(defun w3m-register-desktop-save ()
  "Set `desktop-save-buffer' to a function returning the current URL."
  (setq desktop-save-buffer (lambda (desktop-dirname) w3m-current-url)))
(add-hook 'w3m-mode-hook 'w3m-register-desktop-save)
(defun w3m-restore-desktop-buffer (d-b-file-name d-b-name d-b-misc)
  "Restore a `w3m' buffer on `desktop' load."
  (when (eq 'w3m-mode desktop-buffer-major-mode)
    (let ((url d-b-misc))
      (when url
        (require 'w3m)
        (if (string-match "^file" url)
            (w3m-find-file (substring url 7))
          (w3m-goto-url-new-session url))
        (current-buffer)))))
(add-to-list 'desktop-buffer-mode-handlers '(w3m-mode . w3m-restore-desktop-buffer))


;;;; Editor behavior

;;; We don't need to see the startup message.
(setq inhibit-startup-message t)

;;; Turn off the toolbar.
(tool-bar-mode -1)

;;; Turn off blinking cursor.
(blink-cursor-mode 0)

;;; Respawn the scratch buffer if it is killed (C-x k).
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))
(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))

  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))

  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)

  ;; Since we killed it, don't let caller do that.
  nil)
(setq initial-scratch-message nil)  ;we know what the scratch buffer is for

;;; Set default major mode to be text-mode instead of fundamental-mode.
;;; Although the doc says that default-major-mode is obsolete since
;;; 23.2 and to use major-mode instead, setting major-mode doesn't
;;; work.
(setq default-major-mode 'text-mode)

;;; Default to filename at point for C-x C-f.
(require 'ffap)
(ffap-bindings)
;(setq browse-url-generic-program "/usr/bin/chromium-browser")
;(setq ffap-url-fetcher 'browse-url-generic)
(setq ffap-url-fetcher 'w3m-browse-url)
(setq ffap-machine-p-known 'accept)  ;No pinging
(setq ffap-c-path
      (list
       (getenv "SRC_TREE")
       "~/src/pxpoint"
       "~/src/webservices"
       "/usr/include"
       "/usr/local/include"
       "C:/Program Files/Microsoft Visual Studio 10.0/VC/include"
       "C:/Program Files/Java/jdk1.6.0_20/include"))

;;; Enable menu of recently opened files.
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(recentf-mode 1)

;;; Uniquely indentify buffers
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; Display line numbers.
(require 'linum)
(setq linum-format "% 5d")  ;always 5 columns
(global-linum-mode)         ;all buffers
(defun linum-on ()
  (unless (or
           (minibufferp)    ;except the minibuffer
           (string-match "^\\*" (buffer-name (current-buffer))))  ;except special buffers
    (linum-mode 1)))

;;; Put column number in mode line.
(column-number-mode 1)

;;; On-the-fly spell checking.
(add-hook 'text-mode-hook 'turn-on-flyspell)

;;; Key bindings.
(global-set-key (kbd "C-z") 'undo)   ;overrides suspend-frame
(global-set-key (kbd "C-S-z") 'redo)

;;; Mouse yank commands yank at point instead of at click.
(setq mouse-yank-at-point t)

;;; Delete selected text when typing.
(delete-selection-mode 1)

;;; Vertical motion starting at EOL line keeps to EOL.
(setq track-eol t)

;;; Indentation should insert spaces, not tabs.
(setq-default indent-tabs-mode nil)

;;; If we do see tabs, they are 4 chars wide.
(setq default-tab-width 4)

;;; Scroll one line at a time instead of paging.
;;; Paging is what PgUp and PgDn are for.
(setq scroll-conservatively 100)

;;; Remember and restore point location after PgUp,PgDn
(setq scroll-preserve-screen-position t)

;;; Advanced highlighting of matching parenthesis.
(require 'mic-paren)
(paren-activate)
(setq paren-sexp-mode t)  ; Always highlight the whole s-expression.
(add-hook 'LaTeX-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1))))
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (paren-toggle-open-paren-context 1))))

;;; Enable switching between buffers using substrings.
(iswitchb-mode 1)

;;; Put all backups in one directory.
(setq backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
(defun make-backup-file-name (file)
  (concat "~/.emacs.d/backups/" (file-name-nondirectory file) "~"))
(setq auto-save-list-file-prefix "~/.emacs.d/backups/")

;;; Provide an easy goto-line (^C-g).
(global-set-key (kbd "C-c g") 'goto-line)

;;; Save and restore point (F3, F4).
(define-key global-map (kbd "C-<f3>") '(lambda () (interactive) (point-to-register 33)))  ;^F3 Save
(define-key global-map (kbd "<f3>") '(lambda () (interactive) (jump-to-register 33)))      ;F3 Restore
(define-key global-map (kbd "C-<f4>") '(lambda () (interactive) (point-to-register 34)))  ;^F4 Save
(define-key global-map (kbd "<f4>") '(lambda () (interactive) (jump-to-register 34)))      ;F4 Restore

;;; Insert datetime into current buffer (^C-d).
(defun insert-date ()
  "Inserts date time string into current buffer."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d %H:%M:%S")))
(global-set-key (kbd "C-c d") 'insert-date)


;;;; Printing
(require 'ps-print)
(setq ps-number-of-columns 2)
(setq ps-landscape-mode t)
(setq ps-line-number t)
(setq ps-print-color-p nil)
(setq ps-print-header nil)
(setq lpr-page-header-switches '("-F" "--length=61" "--indent=4"))


;;;; Email
;;; Outgoing mail
(require 'smtpmail)
(cond ((string= system-name "deadeye") (setq system-realm "schutter.home"))
      ((string= system-name "missy") (setq system-realm "schutter.home"))
      ((string= system-name "penguin") (setq system-realm "schutter.home"))
      ((string= system-name "pepsi") (setq system-realm "schutter.home"))
      ((string= system-name "pixel") (setq system-realm "schutter.home"))
      ((string= system-name "wampi") (setq system-realm "isc"))
      (t (setq system-realm "unknown")))
(cond ((equal system-realm "isc")
       (setq smtpmail-smtp-server "fdsvdfw01vxms01.infosolco.com")
       (setq smtpmail-local-domain "corelogic.com")
       (setq user-mail-address "tschutter@corelogic.com"))
      ((equal system-realm "iscp")
       (setq smtpmail-smtp-server "smtp.corelogic.com")
       (setq smtpmail-local-domain "corelogic.com")
       (setq user-mail-address "tschutter@corelogic.com"))
      ((equal system-realm "schutter.home")
       (setq smtpmail-smtp-server "smtp.schutter.home")
       (setq smtpmail-local-domain "schutter.home")
       (setq user-mail-address "t.schutter@comcast.net"))
      (t
       (setq smtpmail-smtp-server "localhost")
       (setq user-mail-address "t.schutter@comcast.net")))
;(setq smtpmail-debug-info t)  ;uncomment to debug problems

;;; Use Message to compose mail.
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it)
(add-hook 'message-mode-hook 'turn-on-auto-fill) ;word wrap

;;; LBDB (abook) integration.
(autoload 'lbdb "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-region "lbdb" "Query the Little Brother's Database" t)
(autoload 'lbdb-maybe-region "lbdb" "Query the Little Brother's Database" t)
(add-hook 'message-setup-hook
          (lambda ()
            (require 'lbdb-complete)
            (define-key message-mode-map (kbd "C-c TAB") 'lbdb-complete)
            ))


;;;; Source code manipulation

;;; Display and cleanup bogus whitespace.
(require 'whitespace)
(setq whitespace-style '(face tabs trailing indentation empty))
(global-whitespace-mode 1)
(setq whitespace-action '(auto-cleanup))

;;; Line wrap regions, function definitions, and function calls.
(defun region-line-wrap ()
  "Line wrap region, breaking at commas"
  (let ((newline (if (eq major-mode (quote vbnet-mode)) " _\n" "\n")))
    (save-excursion
      (save-restriction
        (narrow-to-region (mark) (point))
        (goto-char (point-min))
        (forward-char)
        (if (not (looking-at newline))
            (insert newline))
        (while (re-search-forward "," (point-max) t)
          (if (not (looking-at newline))
              (insert newline)))
        (goto-char (point-max))
        (backward-char)
        (beginning-of-line)
        (if (not (looking-at " *)$"))
            (progn
              (goto-char (point-max))
              (backward-char)
              (insert newline)))))
    (indent-region (mark) (point) nil)))
(defun function-line-wrap ()
  "Line wrap function call or function definition"
  (interactive)
  (let ((original-point (point)))
    (save-excursion
      (mark-defun)
      (let ((defun-begin (point)) (defun-end (mark)))
        ;; Try the sexp that we are inside of.
        (goto-char original-point)
        (backward-up-list)
        (if (looking-at "(")
            (progn
              (set-mark (point))
              (forward-list)
              (region-line-wrap))
          ;; Try the sexp before original-point.
          (goto-char original-point)
          (re-search-backward ")" defun-begin)
          (backward-up-list)
          (set-mark (point))
          (forward-list)
          (region-line-wrap))))))
(define-key global-map (kbd "<f2>") '(lambda () (interactive) (function-line-wrap)))


;;;; Python

;;; Python debugging.
;gud-tooltip-mode

;;; Static code checks (either ^C-^W or ^C-^V).
(setq py-pychecker-command "pycheck")
(setq python-check-command "pycheck")

;;; Simplify insertion of debugging print statements.
(load "pyp.el")

;;; Python editing.
(add-hook 'python-mode-hook
          (lambda ()
            (flyspell-prog-mode)  ;on-the-fly spell check in comments
            (define-key python-mode-map (kbd "C-c h") 'pylookup-lookup)  ;lookup in Python doc
            (define-key python-mode-map (kbd "<f12>") 'pyp)  ;insert debug print
            ))

;;; Python doc lookup.  https://github.com/tsgates/pylookup
;; Run "M-x pylookup-update-all" to update database.
(require 'pylookup)
(setq pylookup-dir "~/.emacs.d/pylookup")
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))  ;executable
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))  ;database
(setq pylookup-html-locations '("/usr/share/doc/python2.7/html"))  ;doc source
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

;;; Python ropemacs refactoring.
(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (setq ropemacs-enable-shortcuts nil)
  (pymacs-load "ropemacs" "rope-")
  (define-key ropemacs-local-keymap (kbd "M-/") 'rope-code-assist)
  (define-key ropemacs-local-keymap (kbd "C-c C-d") 'rope-show-doc)
  (define-key ropemacs-local-keymap (kbd "C-c C-g") 'rope-goto-definition)
  (define-key ropemacs-local-keymap (kbd "C-c C-f") 'rope-find-occurrences)
  ;; Automatically save project python buffers before refactorings.
  (setq ropemacs-confirm-saving nil))
(global-set-key "\C-xpl" 'load-ropemacs)


;;;; C++
;(add-hook 'c++-mode-hook
;          (lambda ()
;            (flyspell-prog-mode)
;                                        ; ...
;            ))


;;;; reStructuredText documents.
;;; Ubuntu drops the .py extensions on the rst programs in python-docutils.
(setq rst-compile-toolsets
  '((html . ("rst2html" ".html" nil))
    (latex . ("rst2latex" ".tex" nil))
    (newlatex . ("rst2newlatex" ".tex" nil))
    (pseudoxml . ("rst2pseudoxml" ".xml" nil))
    (xml . ("rst2xml" ".xml" nil))))


;;;; OpenSCAD files.
(autoload 'scad-mode "scad" "Major mode for editing SCAD code." t)
(add-to-list 'auto-mode-alist '("\\.scad$" . scad-mode))
