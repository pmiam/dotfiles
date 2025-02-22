#!/usr/bin/env sh

case $CHEZMOI_ARGS in
    */etcmoi* ) HANDLE_ETCMOI=false;;
    * ) HANDLE_ETCMOI=true;;
esac

if $HANDLE_ETCMOI; then
    echo "applying to /etc!"
    $CHEZMOI_ARGS --persistent-state $CHEZMOI_HOME_DIR/.config/etcmoistate.boltdb\
                  -D /etc -S $CHEZMOI_HOME_DIR/.config/etcmoi
fi
