.PHONY: default
default: test lint format

.PHONY: test
test:
	busted --verbose src

.PHONY: lint
lint:
	luacheck src

.PHONY: format
format:
	stylua .

nix/pkg.nix: aoc2024.cabal
	cabal2nix ./. >$@
