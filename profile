# Executed by sh/ksh for login shells.
# This file should contain commands understood by bash, ksh, and sh.
# Aliases are not inherited by subshells and so should not be defined here.

# OpenBSD
PATH=$HOME/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/local/bin:/usr/local/sbin:/usr/games:.
export PATH HOME TERM

export PKG_PATH=ftp://ftp3.usa.openbsd.org/pub/OpenBSD/5.0/packages/amd64/

# Get the hostname without any domain part.
_HOSTNAME=`hostname`
_HOSTNAME=${HOSTNAME%%.*}

# Set the prompt.
case "$TERM" in
    xterm*|rxvt*)
        # If this is an xterm set the title to host:dir
        PS1='\e]0;${_HOSTNAME}:${PWD#${HOME}/}\a${_HOSTNAME}:\w$ ' 
        ;;
    *)
        PS1='${_HOSTNAME}:\w\$ '
        ;;
esac

# If running bash, include .bashrc if it exists.
if [ -n "$BASH_VERSION" ]; then
    if [ -r "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# If running ksh, include .kshrc if it exists.
if [ -n "$KSH_VERSION" ]; then
    export ENV="$HOME/.kshrc"
fi
