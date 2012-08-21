#!/usr/bin/env python

"""
From http://stefaanlippens.net/svnignorescript
"""

import optparse
import os
import subprocess


def svn_propget(svnprop, path):
    """Fetch a svn property of a file or directory."""
    subproc = subprocess.Popen(
        ["svn", "propget", svnprop, path],
        stdout=subprocess.PIPE
    )
    subproc.wait()
    data = subproc.stdout.read().strip()
    return data


def svn_propset(svnprop, value, path):
    """Set a svn property on a file or directory."""
    subproc = subprocess.Popen(["svn", "propset", svnprop, value, path])
    subproc.wait()


def main():
    """main"""
    option_parser = optparse.OptionParser(
        usage="usage: %prog FILE|DIR [FILE|DIR...]\n" +
            "  Ignore subversion files and dirs."
    )
    option_parser.add_option(
        "--no-sort",
        action="store_false",
        dest="sort",
        default=True,
        help="do not sort property values, just append"
    )

    (options, args) = option_parser.parse_args()
    if len(args) == 0:
        option_parser.error("no files or dirs specified")

    for path in args:
        dirpath, filename = os.path.split(path)
        svnignore_data = svn_propget("svn:ignore", dirpath)

        if filename not in svnignore_data.split("\n"):
            svnignore_data += "\n" + filename
            if options.sort:
                svnignore_data = "\n".join(sorted(svnignore_data.split("\n")))
            svn_propset("svn:ignore", svnignore_data, dirpath)

if __name__ == "__main__":
    main()
