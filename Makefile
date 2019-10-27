check-design-system:
	unbuffer elm make --output=/dev/null src/DesignSystem.elm 2>&1 | less -r

check-app:
	unbuffer elm make --output=/dev/null src/Main.elm 2>&1 | less -r

serve-design-system:
	yarn parcel serve ./design-system.html

build-app:
	yarn parcel build ./index.html

serve-app: build-app
	PORT=2345 go run server.go


test:
	elm-test
