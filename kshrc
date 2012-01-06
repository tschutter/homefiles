# Executed by ksh for interactive shells.
# This file should contain ksh-only commands.

# Emacs history editing.
set -o emacs

# History navigation.
bind '^XH'=beginning-of-line # home
bind '^XF'=end-of-line # end
bind '^[[3'=prefix-2 # del
bind '^[[3~'=delete-char-forward # del

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

# Source aliases.
if [ -f ~/.aliases ]; then
    . ~/.aliases
fi  
