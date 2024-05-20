# dotfiles
Configuration files, scripts, customizations, and tooling


## Building Docker
I've provided a Dockerfile here to test portability of my Emacs configuration.
This also allows for easy testing of my configuration without having to overwrite your own configuration.

``` shell
docker build . -t this:one
docker run --rm -it this:one
```

## Building Emacs

### On Linux
Sometimes despite being installed on the system, the `./configure` script can't find which GCC compiler or version of `libgccjit` is installed. To tell it, provide the following flags in your environment or preceeding the invocation of `./configure`.

``` shell
CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10 ./configure \
    --with-native-compilation \
    --with-file-notification=inotify \
    --with-tree-sitter \
    --with-sqlite3 \
    --with-json \
    --with-cairo \
    --with-gnutls \
    --with-harfbuzz \
    --with-x-toolkit=gtk3 \
    --with-xwidgets \
    --with-mailutils \
    CFLAGS="-O3 -march=native -fomit-frame-pointer"
```
