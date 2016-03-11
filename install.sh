#!/usr/bin/env bash

# Clone Emacs settings.
SETTINGS_URL="https://github.com/meliasson/emacs-mini.git"
SETTINGS_DIR="$HOME/.emacs.s"
/usr/bin/env git clone $SETTINGS_URL $SETTINGS_DIR

# Create symlink.
rm $HOME/.emacs.d/init.el
ln -s $SETTINGS_DIR/init.el $HOME/.emacs.d/init.el
