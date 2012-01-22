#!/bin/sh
# Generate a signature for mutt(1).

case ${USERNAME:-UNKNOWN} in
    tom)
        echo "Tom Schutter"
        echo "t.schutter@comcast.net"
        ;;
    tschutter)
        echo "Tom Schutter <tschutter@corelogic.com>"
        echo "phone: 512-977-6822"
        ;;
    *)
        ;;
esac
