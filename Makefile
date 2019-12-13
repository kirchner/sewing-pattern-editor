check:
	unbuffer elm make --output=/dev/null src/Main.elm 2>&1 | less -r

dev:
	yarn dev

build:
	yarn build

check-design-system:
	unbuffer elm make --output=/dev/null src/DesignSystem.elm 2>&1 | less -r

serve-design-system:
	yarn parcel serve ./design-system.html
