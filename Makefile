check:
	unbuffer elm make --output=/dev/null src/frontend/Main.elm 2>&1 | less -r

debug:
	nix-shell default.nix -A debug --run server

stories:
	mkdir -p _debug
	elm make --optimize --output=_debug/stories.js src/frontend/Stories.elm
	cp assets/stories.html _debug

