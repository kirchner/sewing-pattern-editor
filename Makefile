check:
	unbuffer elm make --output=/dev/null src/frontend/Main.elm 2>&1 | less -r

debug:
	nix-shell default.nix -A debug --run server

preview:
	nix-shell default.nix -A preview --run server

build:
	nix-build -A dockerImage
