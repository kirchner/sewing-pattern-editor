.PHONY: check debug preview build

check:
	unbuffer elm make --output=/dev/null src/frontend/Main.elm 2>&1 | less -r

debug:
	`nix-build -A debug`

preview:
	`nix-build -A preview`

build:
	nix-build -A dockerImage
