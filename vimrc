" use indents of 4 spaces, and have them copied down lines
set tabstop=4
set shiftwidth=4
set shiftround
set expandtab
set autoindent
set smartindent

" enable syntax highlighting
if has("syntax")
    syntax on
endif

" print the line number in front of each line
set number

" search for text as you enter it
set incsearch

" show matching brackets
set showmatch

if $COMPUTERNAME == "FDSVBLD01W70085" && has("gui_running")
    " Rick, Ashish, and Adam prefer light on dark
    colorscheme desert

    " Rick prefers a larger font
    set guifont=Lucida_Console:h14:cANSI
endif
