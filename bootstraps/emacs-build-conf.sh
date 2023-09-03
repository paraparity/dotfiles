#!/bin/bash

################################################################################
# Bootstrap commands for building Emacs from source
################################################################################
git checkout emacs-29
git pull

./autogen.sh
CFLAGS="-ggdb3 -O0" CXXFLAGS="-ggdb3 -O0" LDFLAGS="-ggdb3"  ./configure \
    --with-native-compilation \
    --with-json \
    --with-cairo \
    --with-modules
