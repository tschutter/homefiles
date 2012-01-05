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

# Alias definitions.
if [ "${_UNAME}" = "OpenBSD" ]; then
    alias dir="/bin/ls -la"
else
    alias dir="/bin/ls -lab"  # -b = C-style escapes for nongraphic characters
fi
[ -x "/usr/bin/lynx" ] && alias lynx="/usr/bin/lynx -accept_all_cookies"
[ -x "/usr/bin/less" ] && alias more="/usr/bin/less"
