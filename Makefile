.PHONY: default
default: test format

.PHONY: test
test:
	busted .

.PHONY: format
format:
	stylua .
