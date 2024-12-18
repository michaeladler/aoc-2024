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
