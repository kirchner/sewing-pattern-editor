build:
	`nix-build shell.nix`/bin/elm make src/Main.elm --output elm.js --debug

docs:
	`nix-build shell.nix`/bin/elm make --optimize --docs=documentation.json
