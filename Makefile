build:
	`nix-build shell.nix`/bin/elm make src/Main.elm --output elm.js

docs:
	`nix-build shell.nix`/bin/elm make --optimize --docs=documentation.json
