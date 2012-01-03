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


def link(options, file):
    """Creates a symbolic link from homeDir/.file to dotfilesDir/file
    """

    # Determine the source and destination pathnames.
    src = os.path.join(options.dotfilesDir, file)
    dst = os.path.join(options.homeDir, "." + file)

    # The src file or directory should always exist.
    if not os.path.exists(src):
        print "ERROR: File '%s' does not exist." % src
        sys.exit(1)

    if os.path.islink(dst):
        # The destination already exists as a symbolic link.  Delete it if
        # --force or if it points to the wrong place.
	try:
            samefile = os.path.samefile(src, dst)
        except Exception:
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
    linkTarget = os.path.relpath(src, options.homeDir)

    # Make the symbolic link from dst to src.
    print "Creating symbolic link from '%s' to '%s'." % (dst, linkTarget)
    os.symlink(linkTarget, dst)


def main():
    optionParser = OptionParser(
        usage = "usage: %prog [options]\n" +
            "  Installs dotfiles using symbolic links."
    )
    optionParser.add_option(
        "--home-dir",
        action="store",
        dest="homeDir",
        metavar="DIR",
        default=os.path.expanduser("~"),
        help="specify directory to install to (default=%default)"
    )
    optionParser.add_option(
        "--force",
        action="store_true",
        dest="force",
        default=False,
        help="replace existing symbolic links"
    )
    optionParser.add_option(
        "--verbose",
        action="store_true",
        dest="verbose",
        default=False,
        help="produce more verbose output"
    )

    (options, args) = optionParser.parse_args()
    if len(args) != 0:
        optionParser.error("invalid argument")
    options.dotfilesDir=os.path.expanduser("~/.dotfiles")  # TODO: use arg0 instead

    link(options, "mg")
    link(options, "tmux.conf")
    link(options, "xzgvrc")

    return 0

if __name__ == "__main__":
    sys.exit(main())
