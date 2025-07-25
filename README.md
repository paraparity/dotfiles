# dotfiles

Configuration files, scripts, customizations, and tooling


## Building Docker

I've provided a Dockerfile here to test portability of my Emacs configuration.
This also allows for easy testing of my configuration without having to overwrite your own configuration.

``` shell
docker build . -t this:one
docker run --rm -it this:one
```

## Building Emacs from Source

For the purpose of this document I only explore building on Linux, namely Ubuntu

### Dotfile Dependencies

``` shell
sudo apt-get update
sudo apt-get install \
    build-essential \
	autoconf \
	pandoc \
	texinfo \
```

#### Install Fonts

I enjoy using FiraCode but this isn't available out of the box always. On Ubuntu, it can be installed with:

``` shell
sudo add-apt-repository universe
sudo apt-get install fonts-firacode
```

### Build Prerequisites

I like to configure Emacs a pretty rich set of features, so there's a lot to install first.

``` shell
sudo apt-get install \
	libgccjit-13-dev \ # or libgccjit-*-dev - depends on your gcc version
	libtree-sitter-dev \
	libsqlite3-dev \
	libcairo2-dev \
	libgnutls28-dev \
	libharfbuzz-dev \
	libmailutils-dev \
	libxpm-dev \
	libgif-dev \
	libtiff-dev \
	libjpeg-dev \
	libwebp-dev \
	librsvg2-dev \
	libncurses-dev \
	liblcms2-dev \
	libgpm-dev \
	libm17n-dev \
	libotf-dev \
	libxft-dev \
	libsystemd-dev
```

### Configuring Emacs

When building from source, you'll need to set up for config and adjust build parameters based on your system.

``` shell
./autogen.sh

# you can also specify a program suffix if building multiple version ie: --program-suffix=30
./configure \
    \ # --with-* config settings
    CFLAGS="-O3 -march=native -fomit-frame-pointer"
```

Sometimes despite being installed on the system, the `./configure` script can't find which GCC compiler or version of `libgccjit` is installed. To tell it, provide the following flags in your environment or preceeding the invocation of `./configure`.

Optionally include the `--disable-gc-mark-trace` option for a ~5% better GC performance at the trade off of having less details if you run into GC related bugs. YMMV.

#### Linux Settings

``` shell
./configure \
    --with-native-compilation \
    --with-file-notification=inotify \
    --with-tree-sitter \
    --with-sqlite3 \
    --with-cairo \
    --with-gnutls \
    --with-harfbuzz \
    --with-mailutils \
    CFLAGS="-O3 -march=native -fomit-frame-pointer"
```

#### MacOS Settings

``` shell
./configure \
    --with-native-compilation \
    --with-file-notification=kqueue \
    --with-tree-sitter \
    --with-sqlite3 \
    --with-cairo \
    --with-gnutls \
    --with-harfbuzz \
    --with-mailutils \
    CFLAGS="-O3 -march=native -fomit-frame-pointer"
```

### Building Emacs with Make

``` shell
make -j$(nproc)
sudo make install

#test
emacs --version
```

#### MacOS Errata

First you'll need to move the Nextstep directory into Mac's Applications folder.

``` shell
mv nextstep/Emacs.app /Applications
```

While I don't recall having to do this in the past, recently I've found `make install` isn't setting Emacs up in a way that puts `emacs` or `emacsclient` on the `$PATH`. Instead I've had to set up wrapper scripts into the app package.

First define the following wrapper files.

`/usr/local/bin/emacs`: https://apple.stackexchange.com/questions/178689

``` shell
#!/bin/sh
/Applications/Emacs.app/Contents/MacOS/Emacs $@
```

`/usr/local/bin/emacsclient`: https://stackoverflow.com/questions/23148787

``` shell
#!/bin/sh
socket_file=$(lsof -c Emacs | grep server | tr -s " " | cut -d' ' -f8)

if [[ $socket_file = "" ]]; then
  # Just run Emacs + args
  /Applications/Emacs.app/Contents/MacOS/Emacs $@ &
else
  /Applications/Emacs.app/Contents/MacOS/bin/emacsclient $@ -n -s $socket_file
fi
```

Then make these executable with: `chmod 755 /usr/local/bin/emacs*`

### Within Emacs

Once the dotfiles are in the right place on the machine, launch Emacs then complete integration with:

```
M-x package-list-packages # install pdf-tools
M-x pdf-tools-install
```

## References

- https://www.emacswiki.org/emacs/BuildingEmacs
- https://mgmarlow.com/words/2022-09-08-building-emacs-mac-os/
