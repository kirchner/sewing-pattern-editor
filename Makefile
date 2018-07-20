build:
	`nix-build shell.nix`/bin/elm make src/Main.elm

docs:
	`nix-build shell.nix`/bin/elm make --optimize --docs=documentation.json
