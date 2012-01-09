# Executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Source alias, function, and prompt definitions for Bourne-derived shells.
if [ -f ~/.bournerc ]; then
    . ~/.bournerc
fi

# Number of trailing directory components to retain when expanding \w in PS1.
# Note that PROMPT_DIRTRIM is a bashism.
PROMPT_DIRTRIM=4

# If this is an xterm set the title to host:dir.
# Note that PROMPT_COMMAND is a bashism.
# According to the Intertubes:
#     ESC]0;stringBEL sets the icon name and window title to string
#     ESC]1;stringBEL sets the icon name to string
#     ESC]2;stringBEL sets the window title to string
# But using ESC]0 does not work if $TERM == screen.
# And using ESC]0 does not work in mintty where $TERM == xterm.
# And nobody uses icons to indicate minimized windows anymore.
PROMPT_COMMAND='echo -ne "\033]2;${HOSTNAME}:${PWD/$HOME/~}\007"'

# Append to the history file, don't overwrite it.
shopt -s histappend

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Platform specific definitions.
if [ "${_UNAME}" = "CYGWIN_NT" ]; then
    # Ignore case while completing
    set completion-ignore-case on

    # Run a batch file.
    launch() {
        ${SYSTEMROOT}/system32/cmd.exe /c start `/usr/bin/cygpath.exe --windows $1`
    }
fi

# Add an "alert" alias for long running commands.  Use like so:
#   long-running-command; alert
# "history" is bash-specific.
[ -x /usr/bin/notify-send ] && alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Enable programmable completion features.
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Source local bashrc.
if [ -f ~/.bashrc-local ]; then
    . ~/.bashrc-local
fi
