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


if (module.hot) {
  module.hot.dispose(() => {
    window.location.reload();
  });
}



if ("serviceWorker" in navigator) {
  navigator.serviceWorker.register("./service-worker.js")
  .then(registration => {
    console.log("[registration] scope: ", registration.scope);

    if (registration.installing) {
      console.log("[registration] installing worker found");

      registration.installing.addEventListener("statechange", () => {
        if (registration.active) {
          console.log(
            "[installing worker] [statechange] worker installed, state: ",
            registration.active.state
          );

          if (registration.active.state == "activated") {
            window.location.reload();
          }
        }
      });
    } else if (registration.active) {
      console.log("[registration] active worker found");

      const app = initElm();

      registration.addEventListener("updatefound", () => {
        console.log("[registration] [updatefound]");

        const newWorker = registration.installing;

        newWorker.addEventListener("statechange", () => {
          console.log("[new worker] [statechange] state: ", newWorker.state);

          if (newWorker.state === "installed") {
            app.ports.onNewWorker.send(null);
          }
        });
      });
    }
  })
  .catch(err => console.error("[registration] failed: ", err));
}


const initElm = () => {
  var app = Elm.Main.init({
    flags: {}
  });

  app.ports.requestSeed.subscribe(() => {
    const crypto = window.crypto || window.msCrypto;

    const randInts = getRandomInts(5);
    app.ports.seedReceived.send([randInts[0], randInts.slice(1)]);
  });

  app.ports.selectAllTextIn.subscribe(id => {
    window.requestAnimationFrame(() => {
      const input = document.getElementById(id);

      if (input !== null) {
        input.select();
      }
    });
  });

  app.ports.storeCache.subscribe(function(data) {
    localStorage.setItem(data.key, data.value);

    setTimeout(function() { app.ports.onStoreChange.send({ key: data.key, value: data.value }); }, 0);
  });

  app.ports.requestCache.subscribe(function(data) {
    var value = localStorage.getItem(data.key);

    if (value !== null) {
      app.ports.onStoreChange.send({ key: data.key, value: value });
    }
  });

  window.addEventListener("storage", function(event) {
    if (event.storageArea === localStorage) {
      app.ports.onStoreChange.send({ key: event.key, value: event.newValue });
    }
  });

  return app;
};

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};
