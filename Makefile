check-design-system:
	unbuffer elm make --output=/dev/null src/DesignSystem.elm 2>&1 | less -r

check-app:
	unbuffer elm make --output=/dev/null src/Main.elm 2>&1 | less -r

serve-app:
	yarn parcel serve index.html

test:
	elm-test
