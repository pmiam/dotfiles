#!/usr/bin/env sh

case $CHEZMOI_ARGS in
    */etcmoi* ) HANDLE_ETCMOI=false;;
    */etc/* ) HANDLE_ETCMOI=true;;
    * ) HANDLE_ETCMOI=false;;
esac

if $HANDLE_ETCMOI; then
    echo "adding /etc files!"
    $CHEZMOI_ARGS -D /etc -S $CHEZMOI_HOME_DIR/.config/etcmoi
    $CHEZMOI_EXECUTABLE $CHEZMOI_COMMAND $CHEZMOI_HOME_DIR/.config/etcmoi
fi
