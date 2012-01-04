#!/usr/bin/python
#
# Copyright (c) 2012-2012 Tom Schutter
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
#    - Redistributions of source code must retain the above copyright
#      notice, this list of conditions and the following disclaimer.
#    - Redistributions in binary form must reproduce the above
#      copyright notice, this list of conditions and the following
#      disclaimer in the documentation and/or other materials provided
#      with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#

"""
Installs dotfiles in tschutter/homefiles using symbolic links.
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
    make_link(options, "mailcap")
    make_link(options, "mg")
    make_link(options, "pylintrc")
    make_link(options, "tmux.conf")
    make_link(options, "xzgvrc")

    if os.uname()[0].startswith("CYGWIN"):
        make_link(options, "minttyrc")

    return 0

if __name__ == "__main__":
    sys.exit(main())
