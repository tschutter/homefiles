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

# Alias definitions.
if [ "${_UNAME}" = "OpenBSD" ]; then
    alias dir="/bin/ls -la"
else
    alias dir="/bin/ls -lab"  # -b = C-style escapes for nongraphic characters
fi
[ -x "/usr/bin/lynx" ] && alias lynx="/usr/bin/lynx -accept_all_cookies"
[ -x "/usr/bin/less" ] && alias more="/usr/bin/less"

# Platform specific definitions.
if [ "${_UNAME}" = "CYGWIN_NT" ]; then
    # Ignore case while completing
    set completion-ignore-case on

    # -W: show windows as well as cygwin processes
    alias ps="ps -W"

    # Run a batch file.
    function launch() {
        ${SYSTEMROOT}/system32/cmd.exe /c start `/usr/bin/cygpath.exe --windows $1`
    }
fi

# Enable programmable completion features.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Source local bashrc.
if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi
