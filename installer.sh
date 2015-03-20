#!/usr/bin/env bash

# Clone Emacs settings.
SETTINGS_URL="https://github.com/meliasson/emacs-mini.git"
SETTINGS_DIR="~/.emacs.s"
/usr/bin/env git clone $SETTINGS_URL $SETTINGS_DIR

# Create symlink.
rm ~/.emacs.d/init.el
ln -s $SETTINGS_DIR/init.el ~/.emacs.d/personal/init.el
