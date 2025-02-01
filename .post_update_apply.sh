#!/usr/bin/env bash

if ! [[ $CHEZMOI_ARGS =~ /etcmoi ]]; then
    echo "applying to /etc!"
    $CHEZMOI_ARGS -D /etc -S $CHEZMOI_HOME_DIR/.config/etcmoi
fi
