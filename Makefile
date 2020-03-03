create:
	scripts/init_database.sh

up:
	nixos-container start sewinghub

down:
	nixos-container stop sewinghub

show:
	nixos-container show-ip sewinghub

destroy:
	nixos-container destroy sewinghub
