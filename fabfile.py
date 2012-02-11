#!/usr/bin/env python

"""
Fabric fabfile for .homefiles.
"""

from fabric.api import cd, env, run
import paramiko
import socket

# Run in a non-interactive mode, calling abort anytime it would
# normally prompt the user for input.
env.abort_on_prompts = True

# We must run the shell as a login shell (-l) so that install.py can
# find files in ~/bin/
env.shell = "/bin/sh -l -c"

env.hosts = [
    "crookshanks",
    "deadeye",
    "missy",
    "molly",
    "pepsi",
    "shadow"
]


def _is_host_up(host, port):
    """Determine if we can connect to host."""
    # Set the timeout
    original_timeout = socket.getdefaulttimeout()
    new_timeout = 3
    socket.setdefaulttimeout(new_timeout)
    host_status = False
    try:
        paramiko.Transport((host, port))
        host_status = True
    except:
        print "[%s] WARNING: Cannot connect to port %s." % (host, port)
    socket.setdefaulttimeout(original_timeout)
    return host_status


#@parallel # waiting for fab-1.3
def deploy():
    """Deploy .homefiles updates to known hosts."""
    if _is_host_up(env.host, int(env.port)):
        homefiles_dir = "~/.homefiles"
        with cd(homefiles_dir):
            run("git pull")
            run("python install.py")
