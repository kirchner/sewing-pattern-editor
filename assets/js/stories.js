// We need to import the CSS so that webpack will load it.
// The MiniCssExtractPlugin is used to separate it out into
// its own CSS file.
import "../css/app.css";
import '@fortawesome/fontawesome-free/js/fontawesome';
import '@fortawesome/fontawesome-free/js/solid';

import { Elm } from "../src/Stories.elm";

var SETTINGS_KEY = "bf_settings";

var app = Elm.Stories.init({
  flags: localStorage.getItem(SETTINGS_KEY),
});

app.ports.saveSettings.subscribe(settings => localStorage.setItem(SETTINGS_KEY, settings));
