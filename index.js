// Sewing pattern editor
// Copyright (C) 2018  Fabian Kirchner <kirchner@posteo.de>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

import { Elm } from './src/Main.elm';
import '@fortawesome/fontawesome-free/css/all.css';

const initElm = () => {
  var app = Elm.Main.init({
    flags: {}
  });

  if (app.ports.selectAllTextIn) {
    app.ports.selectAllTextIn.subscribe(id => {
      window.requestAnimationFrame(() => {
        const input = document.getElementById(id);

        if (input !== null) {
          input.select();
        }
      });
    });
  }

  if (app.ports.storeCache) {
    app.ports.storeCache.subscribe(function(data) {
      localStorage.setItem(data.key, data.value);

      setTimeout(function() {
        app.ports.onStoreChange.send({ key: data.key, value: data.value });
      }, 0);
    });
  }

  if (app.ports.requestCache) {
    app.ports.requestCache.subscribe(function(data) {
      var value = localStorage.getItem(data.key);

      if (value !== null) {
        app.ports.onStoreChange.send({ key: data.key, value: value });
      } else {
        app.ports.onStoreMissing.send({ key: data.key });
      }
    });
  }

  window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage) {
      if (app.ports.onStoreChange) {
        app.ports.onStoreChange.send({ key: event.key, value: event.newValue });
      }
    }
  });

  return app;
};


window.addEventListener("DOMContentLoaded", function() {
  initElm();
});
