#!/usr/bin/env python

# install dotfiles by placing symlinks from ~/ to this git repo
import os, shutil

dotfiles = [".emacs",".cshrc.common",".dir_colors",".Xdefaults",".vimrc"]

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
        shutil.move(target,repl)
    os.symlink(source,target)
