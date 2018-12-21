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
  navigator.serviceWorker.register("/service-worker.js")
    .then(function(registration) {
      console.log("Successfully registered service worker.");

      if (registration.active) {
        console.log("Service worker already active.");

        initElm();
      } else {
        console.log("Service worker not active, yet.");

        registration.onupdatefound = () => {
          const newWorker = registration.installing;

          console.log("A new service worker is being installed");
          console.log(newWorker);

          newWorker.addEventListener("statechange", () => {
            console.log("State changed.");

            if (newWorker.state === "activated") {
              console.log("Service worker is active now.")

              location.reload();
            }
          });
        };
      }
    })
    .catch(function(error) {
      console.log("Registration failed with " + error);
    });
}


const initElm = () => {
  console.log("Initializing Elm application.");

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
};

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};
