.PHONY: default
default: test format

.PHONY: test
test:
	busted --verbose src

.PHONY: format
format:
	stylua .
