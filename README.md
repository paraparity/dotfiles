# dotfiles
Configuration files, scripts, customizations, and tooling


## Building Docker
   I've provided a Dockerfile here to test portability of my Emacs configuration.
   This also allows for easy testing of my configuration without having to overwrite your own configuration.

``` shell
docker build . -t this:one
docker run --rm -it this:one
```
