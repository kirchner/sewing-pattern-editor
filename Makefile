build:
	elm make src/Main.elm --output elm.js

optimized:
	elm make src/Main.elm --optimize --output elm.js

docs:
	elm make --optimize --docs=documentation.json
