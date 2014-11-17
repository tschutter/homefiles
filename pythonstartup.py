"""
Loaded by interactive Python sessions via PYTHONSTARTUP environment
variable.

For occasional use.  If you are looking in here, you probably should
be using ipython instead.

See http://docs.python.org/tutorial/interactive.html
"""

from __future__ import print_function
import atexit
import os.path
import readline
import rlcompleter
import xdg.BaseDirectory

# Including rlcompleter is required for auto-completion.  The assert
# exists to prevent unused import warnings.
assert rlcompleter

# Enable auto-completion with the tab key.
readline.parse_and_bind('tab: complete')
print("Use TAB for auto-completion.")


def _history_pathname():
    """Determine pathname of where history is stored."""
    pathname = os.path.join(
        xdg.BaseDirectory.xdg_cache_home,
        "python",
        "history"
    )
    return pathname


def _save_history():
    """Save history to file."""
    readline.write_history_file(_history_pathname())

atexit.register(_save_history)


def _load_history():
    """Load history from file."""
    history_pathname = _history_pathname()
    if os.path.exists(history_pathname):
        readline.read_history_file(history_pathname)
        print("History loaded from {}.".format(history_pathname))

_load_history()
