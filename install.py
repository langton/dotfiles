#!/usr/bin/env python

# install dotfiles by placing symlinks from ~/ to this git repo
import os, shutil, sys

flags = sys.argv[1:]

if "--help" in flags:
    print "Usage: install.py [flags]\nwhere the valid flags are:"
    print "\t--help   \tDisplay this message and quit"
    print "\t--verbose\tPrint all file operations"
    print "\t--dryrun \tDon't perform file operations (implies --verbose)"
    sys.exit(0)
# Print out file operations, instead of performing them.
dryrun = "--dryrun" in flags
# --dryrun implies --verbose
verbose = dryrun or "--verbose" in flags

dotfiles = [("emacs.el","~/.emacs"),
            ("dir_colors","~/.dir_colors"),
            ("Xdefaults","~/.Xdefaults"),
            ("pystartup.py","~/.pystartup"),
            ("bashrc.sh","~/.bashrc"),
            ("Monaco.ttf","~/.fonts/Monaco.ttf")]

# this script should be in the same directory as the dotfiles
df_path = os.path.dirname(os.path.realpath(__file__))
font_dir = os.path.expanduser("~/.fonts")
if not os.path.exists(font_dir):
    os.mkdir(font_dir)

for d,l in dotfiles:
    dfile = os.path.join(df_path,d)
    link = os.path.expanduser(l)

    # if necessary, move existing file out of the way
    if os.path.lexists(link):
        # move to <filename>.BAK.<i>
        i = 0
        cond = True
        while cond:
            repl = link+".BAK."+str(i)
            cond = os.path.lexists(repl)
            i += 1
        if verbose:
            print "Moving %s to %s" % (link,repl)
        if not dryrun:
            shutil.move(link,repl)
    if verbose:
        print "Creating symlink %s to %s\n--" % (link,dfile)
    if not dryrun:
        os.symlink(dfile,link)
print "Dotfile symlinks installed. Now add the following code to .cshrc:\n"
print "if ( -f $HOME/.cshrc.common ) then"
print "    source $HOME/.cshrc.common"
print "endif\n"
