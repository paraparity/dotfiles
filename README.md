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
	libgccjit-11-dev \ # or libgccjit-10-dev - depends on your gcc version
	libtree-sitter-dev \
	libsqlite3-dev \
	libjansson-dev \
	libcairo2-dev \
	libgnutls28-dev \
	libharfbuzz-dev \
	libgtk-3-dev \
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
	libsystemd-dev \
	libwebkit2gtk-4.0-dev
```

### Configuring Emacs

Sometimes despite being installed on the system, the `./configure` script can't find which GCC compiler or version of `libgccjit` is installed. To tell it, provide the following flags in your environment or preceeding the invocation of `./configure`.

``` shell
./autogen.sh

# you can also specify a program suffix if building multiple version ie: --program-suffix=30
CC=/usr/bin/gcc-11 CXX=/usr/bin/gcc-11 ./configure \
    --with-native-compilation \
    --with-file-notification=inotify \
    --with-tree-sitter \
    --with-sqlite3 \
    --with-cairo \
    --with-gnutls \
    --with-harfbuzz \
    --with-x-toolkit=gtk3 \
    --with-xwidgets \
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

### Within Emacs

Once the dotfiles are in the right place on the machine, launch Emacs then complete integration with:

```
M-x package-list-packages # install pdf-tools
M-x pdf-tools-install
```
