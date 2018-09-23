build:
	elm make src/Main.elm --output elm.js

docs:
	elm make --optimize --docs=documentation.json
