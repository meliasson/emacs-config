#!/usr/bin/env bash

# Clone config.
CONFIG_URL="git@github.com:meliasson/emacs-config.git"
CONFIG_DIR="$HOME/.emacs.c"
/usr/bin/env git clone $SETTINGS_URL $SETTINGS_DIR

# Create directory.
mkdir -p $HOME/.emacs.d

# Create symlink.
if [ -f $HOME/.emacs.d/init.el ]
then
    rm $HOME/.emacs.d/init.el
fi
ln -s $SETTINGS_DIR/init.el $HOME/.emacs.d/init.el
