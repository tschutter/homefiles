# Executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Number of trailing directory components to retain when expanding \w in PS1.
# Note that PROMPT_DIRTRIM is a bashism.
PROMPT_DIRTRIM=4

# If this is an xterm set the title to host:dir.
# Note that PROMPT_COMMAND is a bashism.
case "$TERM" in
    xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${HOSTNAME}:${PWD/$HOME/~}\007"'
        ;;
    screen)
        PROMPT_COMMAND='echo -ne "\033]2;${HOSTNAME}:${PWD/$HOME/~}\007"'
        ;;
    *)
        ;;
esac

# Do not put controlling instructions into history
# export HISTIGNORE="[   ]*:&:bg:fg:exit"

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
#[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Function definitions.
function _cdhelper() {
    CMD="cd $1"
    shift
    for ARG in $*; do
        CMD=${CMD}/${ARG}
    done
    eval ${CMD}
}
function home() {
    _cdhelper ${HOME} $*
}
function src() {
    _cdhelper ${SRC} $*
}
function u() {
    _cdhelper .. $*
}
function uu() {
    _cdhelper ../.. $*
}
function uuu() {
    _cdhelper ../../.. $*
}
function uuuu() {
    _cdhelper ../../../.. $*
}

# Platform dependant definitions.
if [ "${_UNAME}" = "CYGWIN_NT" ]; then
    # Ignore case while completing
    set completion-ignore-case on

    # -e: auto exit the second time we reach EOF
    # -m: prompt verbosely with percent into the file
    alias more="/usr/bin/less -em"

    # -W: show windows as well as cygwin processes
    alias ps="ps -W"

    # Run a batch file.
    function launch() {
        ${SYSTEMROOT}/system32/cmd.exe /c start `/usr/bin/cygpath.exe --windows $1`
    }
fi

# Alias definitions.
if [ "${_UNAME}" = "OpenBSD" ]; then
    alias dir="/bin/ls -la"
else
    alias dir="/bin/ls -lab"  # -b = C-style escapes for nongraphic characters
fi
alias lynx="/usr/bin/lynx -accept_all_cookies"

# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi
