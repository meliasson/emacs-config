#!/usr/bin/env bash

# Clone config.
CONFIG_URL="https://github.com/meliasson/emacs-config.git"
CONFIG_DIR="$HOME/.emacs.c"
/usr/bin/env git clone $CONFIG_URL $CONFIG_DIR

# Create directory.
mkdir -p $HOME/.emacs.d

# Create symlink.
if [ -f $HOME/.emacs.d/init.el ]
then
    rm $HOME/.emacs.d/init.el
fi
ln -s $CONFIG_DIR/init.el $HOME/.emacs.d/init.el
