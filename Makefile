build:
	nix-shell --run "yarn; yarn build"

dev:
	nix-shell --run "yarn; yarn dev"

serve:
	go run server.go
