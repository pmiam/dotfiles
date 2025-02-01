#!/usr/bin/env sh

if [[ $CHEZMOI_ARGS =~ /etc ]] & ! [[ $CHEZMOI_ARGS =~ /etcmoi ]]; then
    echo "adding /etc files!"
    $CHEZMOI_ARGS -D /etc -S $CHEZMOI_HOME_DIR/.config/etcmoi
    $CHEZMOI_EXECUTABLE $CHEZMOI_COMMAND $CHEZMOI_HOME_DIR/.config/etcmoi
fi
