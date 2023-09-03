#!/bin/bash

################################################################################
# Bootstrap commands for building Emacs from source
################################################################################
git checkout emacs-29

./autogen.sh
./configure \
    --with-native-compilation \
    --with-json \
    --with-cairo
