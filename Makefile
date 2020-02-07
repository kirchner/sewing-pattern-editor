check:
	unbuffer elm make --output=/dev/null src/Main.elm 2>&1 | less -r

check-design-system:
	unbuffer elm make --output=/dev/null src/DesignSystem.elm 2>&1 | less -r
