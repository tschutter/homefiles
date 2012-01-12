" Use indents of 4 spaces, and have them copied down lines.
set tabstop=4
set shiftwidth=4
set shiftround
set expandtab
set autoindent
set smartindent

" Enable syntax highlighting.
if has("syntax")
    syntax on
endif

" Print the line number in front of each line.
set number

" Display line number and column number on right side of modeline.
set ruler

" Search for text as you enter it.
set incsearch

" Show matching brackets.
set showmatch

if $COMPUTERNAME == "FDSVBLD01W70085" && has("gui_running")
    " Rick, Ashish, and Adam prefer light on dark.
    colorscheme desert

    " Rick prefers a larger font.
    set guifont=Lucida_Console:h14:cANSI
endif

" Quickref.

" To use the system clipboard to copy and paste, prepend "+ to your
" y (yank) or p (paste) command.  Use :reg to see the contents of
" the system clipboard.
