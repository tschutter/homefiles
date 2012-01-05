# Executed by ksh for interactive shells.
# This file should contain ksh-only commands.

# Emacs history editing.
set -o emacs

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
