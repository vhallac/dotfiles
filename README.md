# Dotfiles for $HOME

This repository is used to keep all dotfiles in my home directory. Since I also
have other git repositories in my home directory, the repository should not be
cloned directly.

## How to set up the home directry on a new machine

When you are setting up a new mchine, first initialize `$HOME`:

    git init --separate-git-dir=./dotfiles.git

Then, add the remote to this newly initialized git file

    git remote add origin git://github.com/vhallac/dotfiles.git

Finally, do a `git pull` to receive the contents.

Normally, there will be a lot of untracked git files in your home directory, and they will clutter the output of git status. You can suppress these untracked files by using:

    git config --local status.showUntracekdFiles no
