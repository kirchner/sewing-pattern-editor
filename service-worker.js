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

import idb from 'idb';
import { Elm } from './src/Worker.elm';


self.addEventListener("install", event => {
  self.skipWaiting();
});

self.addEventListener("activate", event => {
  event.waitUntil(dbPromise);
});


self.addEventListener("fetch", function(event) {
  var url = new URL(event.request.url);
  var segments = url.pathname.split("/");

  if (segments[1] !== "api") {
    event.respondWith(fetch(event.request));
  } else {
    event.respondWith(
      event.request.text().then(body => {
        const crypto = self.crypto || self.msCrypto;
        const randInts = getRandomInts(5);

        var worker = Elm.Worker.init({
          flags: {
            "seed": randInts[0],
            "seedExtension": randInts.slice(1)
          }
        });

        worker.ports.log.subscribe(line => {
          console.log(line);
        });


        worker.ports.dbGetAll.subscribe(dbRequest => {
          dbPromise.then(db => {
            return db.transaction(dbRequest.storeNames)
              .objectStore(dbRequest.objectStore)
              .getAll();
          })
          .then(payload =>
            worker.ports.onDbResponse.send({
              "uuid": dbRequest.uuid,
              "data": payload
            }))
          .catch(err => console.error(err));
        });

        worker.ports.dbGet.subscribe(dbRequest => {
          dbPromise.then(db => {
            return db.transaction(dbRequest.storeNames)
              .objectStore(dbRequest.objectStore)
              .get(dbRequest.key);
          })
          .then(payload =>
            worker.ports.onDbResponse.send({
              "uuid": dbRequest.uuid,
              "data": payload
            }))
          .catch(err => console.error(err));
        });

        worker.ports.dbAdd.subscribe(dbRequest => {
          dbPromise.then(db => {
            return db.transaction(dbRequest.storeNames, "readwrite")
              .objectStore(dbRequest.objectStore)
              .add(dbRequest.data);
          })
          .then(payload =>
            worker.ports.onDbResponse.send({
              "uuid": dbRequest.uuid,
              "data": payload
            }))
          .catch(err => console.error(err));
        });

        worker.ports.dbPut.subscribe(dbRequest => {
          dbPromise.then(db => {
            return db.transaction(dbRequest.storeNames, "readwrite")
              .objectStore(dbRequest.objectStore)
              .put(dbRequest.data);
          })
          .then(payload =>
            worker.ports.onDbResponse.send({
              "uuid": dbRequest.uuid,
              "data": payload
            }))
          .catch(err => console.error(err));
        });

        worker.ports.dbDelete.subscribe(dbRequest => {
          dbPromise.then(db => {
            return db.transaction(dbRequest.storeNames, "readwrite")
              .objectStore(dbRequest.objectStore)
              .delete(dbRequest.key);
          })
          .then(payload =>
            worker.ports.onDbResponse.send({
              "uuid": dbRequest.uuid,
              "data": payload
            }))
          .catch(err => console.error(err));
        });


        return new Promise(resolve => {
          worker.ports.sendJsonResponse.subscribe(response => {
            resolve(
              new Response(
                new Blob(
                  [JSON.stringify(response.payload)],
                  { type: "application/json" }
                ), {
                  status: response.statusCode,
                  statusText: response.statusText
                }
              )
            );
          });

          worker.ports.onRequest.send({
            "url": event.request.url,
            "method": event.request.method,
            "body": body
          });
        });
      })
      .catch(err => console.error(err))
    );
  }
});

//----

const dbPromise = idb.open("sewing-pattern-editor", 1, upgradeDB => {
  upgradeDB.createObjectStore("patterns", { keyPath: "slug" });
});

const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};
