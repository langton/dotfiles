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

dotfiles = [".emacs",".cshrc.common",".dir_colors",".Xdefaults"]

# this script should be in the same directory as the dotfiles
df_path = os.path.dirname(os.path.realpath(__file__))

home = os.path.expanduser("~/")
for file in dotfiles:
    target = os.path.join(home,file)
    source = os.path.join(df_path,file)

    # if necessary, move existing file out of the way
    if os.path.exists(target):
        # move to <filename>.BAK.<i>
        i = 0
        cond = True
        while cond:
            repl = os.path.join(home,file+".BAK."+str(i))
            cond = os.path.exists(repl)
            i += 1
        if verbose:
            print "Moving %s to %s" % (target,repl)
        if not dryrun:
            shutil.move(target,repl)
    if verbose:
        print "Creating symlink from %s to %s\n--" % (source,target)
    if not dryrun:
        os.symlink(source,target)
print "Dotfile symlinks installed. Now add the following code to .cshrc:\n"
print "if ( -f $HOME/.cshrc.common ) then"
print "    source $HOME/.cshrc.common"
print "endif\n"
