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
