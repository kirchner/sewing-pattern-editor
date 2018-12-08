dev:
	nix-shell --run "yarn; yarn dev"

build:
	nix-shell --run "yarn; yarn build"

serve:
	go run server.go
