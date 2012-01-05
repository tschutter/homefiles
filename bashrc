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
_cdhelper() {
    CMD="cd $1"
    shift
    for ARG in $*; do
        CMD=${CMD}/${ARG}
    done
    eval ${CMD}
}
home() {
    _cdhelper ${HOME} $*
}
src() {
    _cdhelper ${SRC} $*
}
u() {
    _cdhelper .. $*
}
uu() {
    _cdhelper ../.. $*
}
uuu() {
    _cdhelper ../../.. $*
}
uuuu() {
    _cdhelper ../../../.. $*
}

# Platform specific definitions.
if [ "${_UNAME}" = "CYGWIN_NT" ]; then
    # Ignore case while completing
    set completion-ignore-case on

    # Run a batch file.
    launch() {
        ${SYSTEMROOT}/system32/cmd.exe /c start `/usr/bin/cygpath.exe --windows $1`
    }
fi

# Enable programmable completion features.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Source aliases.
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi

# Source local bashrc.
if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi
