---
sitetitle: probabilistic dynamic semantics
pagetitle: installation
---

## Installation

You can install the library and open a repl by cloning [this repository](https://github.com/probabilistic-dynamic-semantics/pds) and doing:

```
cd pds
cabal v2-repl --repl-options="-ghci-script .ghci"
```

A [Nix](https://nixos.org/)-based installation is also possible by doing:

```
cd pds
nix-shell --run "cabal v2-repl --repl-options=\"-ghci-script .ghci\""
```
