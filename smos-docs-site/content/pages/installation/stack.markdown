---
title: Building with stack
description: Documentation about installing Smos using stack
---

It is possible to build smos using [Stack](https://haskellstack.org), but this method is mainly available for development purposes.
In practice we recommend using Nix for installing Smos.


1. Clone the repository:

   ```
   git clone https://github.com/NorfairKing/smos
   ```

1. Install prerequisites:

   ```
   stack install autoexporter
   ```

1. Install Smos with the default configuration:

   ```
   stack install smos
   ```

1. You probably want to also install related tools:

   ```
   stack install smos-query
   stack install smos-archive
   stack install smos-single
   stack install smos-sync-client
   ```

#### Autoconf

You may get an error about `autoreconf` not being available if `autoconf` is not installed.
Make sure that you have `autoreconf` on your path.

#### Arch linux

*Do not install `stack` or any other haskell packages using pacman.*
This is unlikely to build at all and will most likely cause in bugs if it does.
Instead, install `stack` by following the documentation [on this page](https://docs.haskellstack.org/en/stable/README/#how-to-install)
