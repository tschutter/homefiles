#!/usr/bin/env python

"""
Fabric fabfile for .homefiles.
"""

from fabric.api import cd, env, run

# We must run the shell as a login shell (-l) so that install.py can
# find files in ~/bin/
env.shell = "/bin/sh -l -c"

env.hosts = ["deadeye", "molly", "shadow"]
# TODO: add missy, pepsi if pingable


def deploy():
    """Deploy .homefiles updates to known hosts."""
    homefiles_dir = "~/.homefiles"
    with cd(homefiles_dir):
        run("git pull")
        run("python install.py")
