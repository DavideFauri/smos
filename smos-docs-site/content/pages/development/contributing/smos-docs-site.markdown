---
title: Documentation Site
description: How to contribute to the Smos Documentation Site
---

## Setting up a feedback loop

To contribute to the documentation site, use `feedback`:

```
$ nix-shell
nix-shell $ feedback docs
```

## Contributing to the NixOS Module Docs

Building [the NixOS module docs](/nix/nixos-module) or [the Nix home manager module docs](/nix/home-manager-module) require a nix build, so they are not built in the default feedback loop for `smos-docs-site`.
If you want to work on them from the `stack` feedback loop anyway, you can build the module docs with these commands before running the feedback loop:

* `nix-build nix/pkgs.nix -A nixosModuleDocs` and then set `NIXOS_MODULE_DOCS=result/share/doc/nixos/options.json`
* `nix-build nix/pkgs.nix -A homeManagerModuleDocs` and then set `HOME_MANAGER_MODULE_DOCS=result/share/doc/nixos/options.json`

```
feedback docs
```

Note that you may have to `stack clean smos-docs-site` to get the build to pick up this change.
