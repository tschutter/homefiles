#!/usr/bin/env python

"""
Installs files in tschutter/homefiles using symbolic links.
"""

import optparse
import os
import shutil
import stat
import sys
if sys.version_info >= (2, 4):
    import subprocess
import tempfile
import time


def run_command(args, stdinstr):
    """Run an external command, returning stdout and stderr as a string."""
    if sys.version_info < (2, 4):
        print "WARNING: Not running %s" % " ".join(args)
    else:
        process = subprocess.Popen(
            args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE
        )
        stdoutdata, stderrdata = process.communicate(stdinstr)
        return stdoutdata + stderrdata


def cygpath_w(pathname):
    """Converts a pathname to Windows style X:\dir\file."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--windows", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip()
        pipe.close()

    return pathname


def cygpath_u(pathname):
    """Converts a pathname to Cygwin style /cygpath/X/dir/file."""
    if sys.platform == "cygwin":
        pipe = subprocess.Popen(
            ["cygpath", "--unix", pathname],
            stdout=subprocess.PIPE
        ).stdout
        for line in pipe:
            pathname = line.strip()
        pipe.close()

    return pathname


def simplify_path(path):
    """Perform the inverse of os.path.expanduser()."""
    homedir = os.path.expanduser("~")
    path = os.path.abspath(path)
    if path.startswith(homedir):
        path = os.path.join("~", path[len(homedir) + 1:])
    return path


def create_vardir(options):
    """Create ~/.var directory."""
    if os.path.isdir(options.vardir):
        return
    print "Creating '%s' directory." % simplify_path(options.vardir)
    if not options.dryrun:
        os.mkdir(options.vardir, 0700)


def file_in_path(filename):
    """Returns True if filename is in PATH."""
    # Get the PATH.
    path = os.getenv("PATH", os.defpath)

    # Add ~/bin in case this is not a login shell.
    path = "%s%s%s" % (path, os.pathsep, os.path.expanduser("~/bin"))

    # Loop through all of the path components searching for filename.
    for pathdir in path.split(os.pathsep):
        pathname = os.path.join(pathdir, filename)
        if os.path.exists(pathname):
            return True
    return False


def clean_link(options, linkname, backup=True):
    """Delete link or backup files and dirs."""
    link_pathname = os.path.join(options.homedir, linkname)

    if os.path.islink(link_pathname):
        # The destination exists as a symbolic link.
        print "Deleting symbolic link '%s'." % link_pathname
        if not options.dryrun:
            os.unlink(link_pathname)

    elif os.path.exists(link_pathname):
        if os.path.isdir(link_pathname):
            if len(os.listdir(link_pathname)) == 0:
                print "Removing empty directory '%s'." % link_pathname
                if not options.dryrun:
                    os.rmdir(link_pathname)
                    return

        # The destination exists as a file or dir.  Back it up.
        if backup:
            print "Moving '%s' to '%s'." % (link_pathname, options.homefiles)
            if not options.dryrun:
                shutil.move(link_pathname, options.homefiles)
        else:
            print "Deleting file or directory '%s'." % link_pathname
            if not options.dryrun:
                os.unlink(link_pathname)


def make_link(options, enabled, filename, linkname=None):
    """If enabled is True, create a symbolic link from home/linkname to
    homefiles/filename.

    If linkname is not specified, it is the same as filename.
    """

    if linkname == None:
        linkname = filename

    # Determine the source and destination pathnames.
    file_pathname = os.path.join(options.homefiles, filename)
    link_pathname = os.path.join(options.homedir, linkname)

    # The target filename should always exist.
    if not os.path.exists(file_pathname):
        print "ERROR: File '%s' does not exist." % file_pathname
        sys.exit(1)

    if enabled and not options.force and os.path.islink(link_pathname):
        # The destination already exists as a symbolic link.  Delete it if
        # it points to the wrong place.
        try:
            samefile = os.path.samefile(file_pathname, link_pathname)
        except OSError:
            samefile = False
        if not samefile:
            clean_link(options, linkname)
        else:
            if options.verbose:
                print "Link already exists from '%s' to '%s'." % (
                    link_pathname,
                    file_pathname
                )
            return
    else:
        clean_link(options, linkname)

    if not enabled:
        if options.verbose:
            print "Not linking to '%s'." % filename
        return

    # Make the link target relative.  This usually makes the link
    # shorter in ls output.
    if sys.version_info < (2, 6):
        link_target = file_pathname
    else:
        filedir = os.path.dirname(filename)
        link_target = os.path.relpath(
            file_pathname,
            os.path.join(options.homedir, filedir)
        )

    # Make the symbolic link from link_pathname to link_target.
    print "Creating symbolic link from '%s' to '%s'." % (
        link_pathname,
        link_target
    )
    if not options.dryrun:
        os.symlink(link_target, link_pathname)


def make_dot_link(options, enabled, filename):
    """Create a symbolic link from home/.filename to homefiles/filename.
    """
    return make_link(options, enabled, filename, "." + filename)


def make_sig_link(options):
    """Create a link to the appropriate signature file."""
    # Determine the realm.
    computername = os.uname()[1].lower()
    prefix = computername[:7]
    if prefix == "fdsvbld":
        realm = "ISC"
    elif prefix == "fdsvdfw":
        realm = "ISCP"
    elif prefix == "fdsvmad":
        realm = "ISC"
    elif prefix == "fdsvsna":
        realm = "ISCP"
    elif computername in ["apple", "passion", "wampi", "wampi-win2003"]:
        realm = "ISC"
    else:
        realm = "HOME"

    # Link the correct signature.
    if realm in ["ISC", "ISCP"]:
        make_link(options, True, "signature-corelogic", ".signature")
    else:
        make_link(options, True, "signature-home", ".signature")


def create_dotless(options, enabled):
    """
    Create ~/.homefiles/.less file.

    The lesskey program creates the .less file.
    """

    dotless_pathname = os.path.join(options.homefiles, "less")
    lesskey = [
        "#env",
        "LESSHISTFILE=%s" % os.path.join(options.vardir, "less_history")
    ]

    if enabled:
        if options.force or not os.path.exists(dotless_pathname):
            print "Running lesskey to create '%s'." % dotless_pathname
            if not options.dryrun:
                outstr = run_command(
                    ["lesskey", "-o", dotless_pathname, "-"],
                    "\n".join(lesskey)
                )
                if len(outstr.rstrip()) > 0:
                    print outstr.rstrip()
    else:
        clean_link(options, dotless_pathname)


def link_dotfiles(options):
    """Create links in ~ to dotfiles."""

    make_dot_link(options, file_in_path("aspell"), "aspell.en.prepl")
    make_dot_link(options, file_in_path("aspell"), "aspell.en.pws")
    make_dot_link(options, True, "bournerc")
    clean_link(
        options,
        os.path.join(options.homedir, ".bash_history"),
        backup=False
    )
    clean_link(options, os.path.join(options.homedir, ".bash_profile"))
    make_dot_link(options, os.path.exists("/bin/bash"), "bashrc")
    clean_link(options, os.path.join(options.homedir, ".emacs"))
    make_dot_link(options, file_in_path("emacs"), "emacs.d")
    make_dot_link(options, file_in_path("vi"), "exrc")
    make_dot_link(options, file_in_path("git"), "gitconfig")
    make_dot_link(options, os.path.exists("/bin/ksh"), "kshrc")
    make_dot_link(options, file_in_path("lbdbq"), "lbdbrc")
    create_dotless(options, file_in_path("less"))
    make_dot_link(options, file_in_path("less"), "less")
    make_dot_link(
        options,
        file_in_path("mail") or file_in_path("mutt"),
        "mailcap"
    )
    make_dot_link(options, file_in_path("mg"), "mg")
    make_dot_link(options, file_in_path("mintty"), "minttyrc")
    make_dot_link(options, file_in_path("mutt"), "mutt")
    make_dot_link(options, file_in_path("muttprint"), "muttprintrc")
    make_dot_link(options, True, "profile")
    make_dot_link(options, file_in_path("pychecker"), "pycheckrc")
    make_dot_link(options, file_in_path("pdb"), "pdbrc")
    make_dot_link(options, file_in_path("pdb"), "pdbrc.py")
    make_dot_link(options, file_in_path("pylint"), "pylintrc")
    make_dot_link(options, file_in_path("python"), "pythonstartup")
    make_dot_link(options, file_in_path("screen"), "screenrc")
    make_sig_link(options)
    make_dot_link(options, file_in_path("tmux"), "tmux.conf")
    make_dot_link(options, file_in_path("urxvt"), "urxvt")
    make_dot_link(options, file_in_path("valgrind"), "valgrindrc")
    clean_link(
        options,
        os.path.join(options.homedir, ".viminfo"),
        backup=False
    )
    make_dot_link(options, file_in_path("vi"), "vimrc")
    make_dot_link(options, file_in_path("xzgv"), "xzgvrc")
    make_dot_link(options, file_in_path("w3m"), "w3m")
    # Smack the ~/.Xdefaults and ~/.Xresources link if they exist.
    clean_link(options, os.path.join(options.homedir, ".Xdefaults"))
    clean_link(options, os.path.join(options.homedir, ".Xresources"))
    make_dot_link(options, options.is_xwindows, "xsessionrc")


def link_binfiles(options):
    """Create links in ~/bin."""
    bindir = os.path.join(options.homedir, "bin")
    if not os.path.isdir(bindir):
        print "Creating dir '%s'." % bindir
        if not options.dryrun:
            os.mkdir(bindir)
    make_link(options, True, "bin/append-missing-newline")
    make_link(options, True, "bin/find-non-ascii")
    make_link(options, True, "bin/findfile")
    make_link(options, True, "bin/install-essentials")
    make_link(options, True, "bin/pycheck")
    make_link(options, True, "bin/svn-ignore")
    make_link(options, True, "bin/tgrep")
    make_link(options, True, "bin/tm")


def create_tmp_file(prefix, suffix, contents):
    """Create a temporary file containing contents."""
    handle, pathname = tempfile.mkstemp(prefix=prefix, suffix=suffix)
    os.fchmod(handle, stat.S_IRUSR | stat.S_IWUSR | stat.S_IXUSR)
    tmp_file = os.fdopen(handle, "wb")
    tmp_file.write(contents)
    return pathname


def install_fonts(options):
    """Install fonts."""
    # See http://blogs.technet.com/b/heyscriptingguy/archive/\
    # 2008/04/25/how-can-i-install-fonts-using-a-script.aspx
    if options.is_cygwin or options.is_windows:
        src_dir = os.path.join(options.homefiles, "fonts")
        # Need to see what cygwin local and cygwin ssh do here...
        system_root = os.environ["SYSTEMROOT"]
        dst_dir = os.path.join(system_root, "Fonts")
        for filename in os.listdir(src_dir):
            if filename.endswith(".ttf"):
                src_pathname = os.path.join(src_dir, filename)
                dst_pathname = os.path.join(dst_dir, filename)
                dst_pathname = cygpath_u(dst_pathname)
                if not options.force and os.path.exists(dst_pathname):
                    continue
                print "Installing font '%s'." % src_pathname
                if not options.dryrun:
                    vbs_text = (
                        'Set objShell = CreateObject("Shell.Application")\r\n'
                        'Set objFolder = objShell.Namespace(&H14&)\r\n'
                        'objFolder.CopyHere "%s"\r\n' % cygpath_w(src_pathname)
                    )
                    vbs_pathname = create_tmp_file(
                        "install-font",
                        ".vbs",
                        vbs_text
                    )
                    cmd_exe = os.path.join(
                        cygpath_u(system_root),
                        "system32",
                        "cmd.exe"
                    )
                    outstr = run_command(
                        [
                            cmd_exe,
                            "/c",
                            "start",
                            cygpath_w(vbs_pathname)
                        ],
                        None
                    )
                    if len(outstr.rstrip()) > 0:
                        print outstr.rstrip()
                    # We should give the "/wait" parameter to the
                    # start command above.  But that sometimes causes
                    # a hang.  Therefore we just sleep here.
                    time.sleep(5)
                    os.unlink(vbs_pathname)
    else:
        # Note that ttf-ubuntu-font-family 0.71 did not include UbuntuMono.
        system_has_ubuntu_mono = os.path.exists(
            "/usr/share/fonts/truetype/ubuntu-font-family/UbuntuMono-R.ttf"
        )
        make_dot_link(options, not system_has_ubuntu_mono, "fonts")


def main():
    """main"""
    option_parser = optparse.OptionParser(
        usage="usage: %prog [options]\n" +
            "  Installs files in tschutter/homefiles using symbolic links."
    )
    option_parser.add_option(
        "--home",
        action="store",
        dest="homedir",
        metavar="DIR",
        default="~",
        help="specify directory to install to (default=%default)"
    )
    option_parser.add_option(
        "--var",
        action="store",
        dest="vardir",
        metavar="DIR",
        default=os.path.join("~", ".var"),
        help="specify var directory (default=%default)"
    )
    option_parser.add_option(
        "--force",
        action="store_true",
        dest="force",
        default=False,
        help="replace existing files and symbolic links"
    )
    option_parser.add_option(
        "-n",
        "--dry-run",
        action="store_true",
        dest="dryrun",
        default=False,
        help="print commands that would be executed, but do not execute them"
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
    options.homedir = os.path.expanduser(options.homedir)
    options.vardir = os.path.expanduser(options.vardir)

    # Determine what platform we are on.
    options.is_cygwin = sys.platform == "cygwin"
    options.is_windows = sys.platform.startswith("win")
    options.is_xwindows = not (options.is_cygwin or options.is_windows)

    options.homefiles = os.path.dirname(os.path.abspath(__file__))

    create_vardir(options)

    link_dotfiles(options)

    link_binfiles(options)

    install_fonts(options)

    return 0

if __name__ == "__main__":
    sys.exit(main())
