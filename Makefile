build:
	nix-shell --run "yarn build"

dev:
	nix-shell --run "yarn dev"

serve:
	go run server.go
