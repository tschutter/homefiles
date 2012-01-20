#!/usr/bin/env python

"""
Tools to aid in Python debugging.

Installs a sys.excepthook that automatically starts a post-mortem
debugging session if an exception is raised and is uncaught.
"""

import sys


def info(extype, value, tback):
    """Custom sys.excepthook."""
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        # We are in interactive mode or we don't have a tty-like
        # device, so we call the default hook
        sys.__excepthook__(extype, value, tback)
    else:
        import pdb
        import traceback
        # We are not in interactive mode, print the exception.
        traceback.print_exception(extype, value, tback)
        print
        # Start the debugger in post-mortem mode.
        pdb.post_mortem(tback)

sys.excepthook = info
