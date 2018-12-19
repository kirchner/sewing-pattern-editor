build:
	nix-shell --run "yarn build"

dev:
	nix-shell --run "yarn dev"

test-watch:
	nix-shell --run "yarn elm-test --watch"

serve:
	go run server.go
