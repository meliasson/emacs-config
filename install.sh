#!/usr/bin/env bash

# Clone Emacs settings.
SETTINGS_URL="https://github.com/meliasson/emacs-mini.git"
SETTINGS_DIR="$HOME/.emacs.s"
/usr/bin/env git clone $SETTINGS_URL $SETTINGS_DIR

# Create symlink.
if [ -f $HOME/.emacs.d/init.el ]
then
    rm $HOME/.emacs.d/init.el
fi
ln -s $SETTINGS_DIR/init.el $HOME/.emacs.d/init.el
