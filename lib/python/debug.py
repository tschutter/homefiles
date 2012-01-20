#!/usr/bin/env python

"""
Tools to aid in Python debugging.

Installs a sys.excepthook that automatically starts a post-mortem
debugging session if an exception is raised and is uncaught.
"""

import sys


def info(extype, value, traceback):
    """Custom sys.excepthook."""
    if hasattr(sys, 'ps1') or not sys.stderr.isatty():
        # We are in interactive mode or we don't have a tty-like
        # device, so we call the default hook
        sys.__excepthook__(extype, value, traceback)
    else:
        import pdb
        import traceback
        # We are NOT in interactive mode, print the exception.
        traceback.print_exception(extype, value, traceback)
        print
        # Start the debugger in post-mortem mode.
        pdb.post_mortem(traceback)

sys.excepthook = info
