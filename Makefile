check:
	unbuffer elm make --output=/dev/null src/Main.elm 2>&1 | less -r

build:
	yarn build

dev:
	ENVIRONMENT=development PORT=2345 go run server.go

run: build
	ENVIRONMENT=production PORT=2345 go run server.go

check-design-system:
	unbuffer elm make --output=/dev/null src/DesignSystem.elm 2>&1 | less -r

serve-design-system:
	yarn parcel serve ./design-system.html
