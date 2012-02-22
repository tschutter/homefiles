" vim and gvim config file

" Whoever designed the viminfo variable was smoking crack.  All we
" want to do is specify the location of the viminfo file, but to do
" so we must also specify the !, ', and % options as well.
" http://vimdoc.sourceforge.net/htmldoc/options.html#'viminfo'
set viminfo=!,%,'20,n~/.var/viminfo

" Use indents of 4 spaces, and have them copied down lines.
set tabstop=4
set shiftwidth=4
set shiftround
set expandtab

" Although autoindent sounds nice, it totally screws up text pasted
" from other apps.  And in vim, I paste more often than creating
" original text.
set noautoindent
set nosmartindent

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
