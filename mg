; mg (emacs-like text editor) config file

auto-fill-mode
set-fill-column 72
line-number-mode ; display in modeline
global-set-key "\^[[3~" delete-char
global-set-key "\^cg" goto-line
global-set-key "\^hk" describe-key-briefly
global-set-key "\^z" undo
