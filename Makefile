optimized: setup-gh-pages
	mkdir -p assets/js
	elm make src/Main.elm --optimize --output assets/js/elm.js
	uglifyjs assets/js/elm.js \
	    --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
	    | uglifyjs --mangle --output=assets/js/elm.min.js
	mv assets/js/elm.min.js assets/js/elm.js
	cp assets/js/elm.js gh-pages/assets/js/elm.js

dev: setup-gh-pages
	mkdir -p assets/js
	elm make src/Main.elm --output assets/js/elm.js
	cp assets/js/elm.js gh-pages/assets/js/elm.js

setup-gh-pages:
	mkdir -p gh-pages/assets/js
	mkdir -p gh-pages/assets/icons
	cp src/index.html gh-pages/
	cp assets/js/*.js gh-pages/assets/js/
	cp assets/icons/*.svg gh-pages/assets/icons/
