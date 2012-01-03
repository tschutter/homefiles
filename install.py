#!/usr/bin/python
#
# $Id: build.py 9335 2011-06-21 19:31:44Z tschutter $
#
# Copyright 2008-2010 CoreLogic.
# All rights reserved.
#

"""
Installs dotfiles using symbolic links.
"""

from optparse import OptionParser
import os
import shutil
import sys


def make_link(options, name):
    """Creates a symbolic link from homeDir/.name to dotfilesDir/name
    """

    # Determine the source and destination pathnames.
    src = os.path.join(options.dotfilesDir, name)
    dst = os.path.join(options.homeDir, "." + name)

    # The src file or directory should always exist.
    if not os.path.exists(src):
        print "ERROR: File '%s' does not exist." % src
        sys.exit(1)

    if os.path.islink(dst):
        # The destination already exists as a symbolic link.  Delete it if
        # --force or if it points to the wrong place.
        try:
            samefile = os.path.samefile(src, dst)
        except OSError:
            samefile = False
        if options.force or not samefile:
            print "Deleting symbolic link '%s'." % dst
            os.unlink(dst)
        else:
            if options.verbose:
                print "Link already exists from '%s' to '%s'." % (dst, src)
            return

    elif os.path.exists(dst):
        # Backup the existing file or dir.
        print "Moving '%s' to '%s'." % (dst, options.dotfilesDir)
        shutil.move(dst, options.dotfilesDir)

    # Make the link target relative to homeDir.  This usually makes the link
    # shorter in ls output.
    link_target = os.path.relpath(src, options.homeDir)

    # Make the symbolic link from dst to src.
    print "Creating symbolic link from '%s' to '%s'." % (dst, link_target)
    os.symlink(link_target, dst)


def main():
    """main"""
    option_parser = OptionParser(
        usage="usage: %prog [options]\n" +
            "  Installs dotfiles using symbolic links."
    )
    option_parser.add_option(
        "--home-dir",
        action="store",
        dest="homeDir",
        metavar="DIR",
        default=os.path.expanduser("~"),
        help="specify directory to install to (default=%default)"
    )
    option_parser.add_option(
        "--force",
        action="store_true",
        dest="force",
        default=False,
        help="replace existing symbolic links"
    )
    option_parser.add_option(
        "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="produce more verbose output"
    )

    (options, args) = option_parser.parse_args()
    if len(args) != 0:
        option_parser.error("invalid argument")

    options.dotfilesDir = os.path.dirname(os.path.abspath(__file__))

    make_link(options, "gitconfig")
    make_link(options, "mg")
    make_link(options, "pylintrc")
    make_link(options, "tmux.conf")
    make_link(options, "xzgvrc")

    return 0

if __name__ == "__main__":
    sys.exit(main())
