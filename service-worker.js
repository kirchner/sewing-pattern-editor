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

import { Elm } from './src/Worker.elm';

const dbName = "sewing-pattern-editor";
const dbVersion = 1;


self.addEventListener("install", event => {
  self.skipWaiting();
});

self.addEventListener("activate", event => {
  self.skipWaiting();
});


self.addEventListener("fetch", function(event) {
  const url = new URL(event.request.url);
  const segments = url.pathname.split("/");

  if (segments[1] !== "api") {
    event.respondWith(fetch(event.request));
  } else {
    event.respondWith(
      event.request.text().then(body => {
        const crypto = self.crypto || self.msCrypto;
        const randInts = getRandomInts(5);

        const worker = Elm.Worker.init({
          flags: {
            "seed": randInts[0],
            "seedExtension": randInts.slice(1)
          }
        });

        handleLog(worker);
        handleDbRequests(worker);

        return new Promise(resolve => {
          worker.ports.sendJsonResponse.subscribe(response => {
            resolve(
              new Response(
                new Blob(
                  [response.payload],
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


const handleLog = worker => {
  worker.ports.log.subscribe(line => {
    console.log("[service worker] [elm]", line);
  });
};


const handleDbRequests = worker => {
  worker.ports.dbRequest.subscribe(stuff => {
    const dbRequest = indexedDB.open(dbName, dbVersion);

    dbRequest.addEventListener("error",  err => {
      console.error("[service worker] [db]", err);
    });

    dbRequest.addEventListener("upgradeneeded", event => {
      console.log("[service worker] [db] upgrade needed");
      createObjectStore(
        dbRequest.result,
        stuff.info.objectStore,
        stuff.info.keyPath
      );
    });

    dbRequest.addEventListener("success", event => {
      const db = dbRequest.result;

      db.addEventListener("error", err => {
        console.error("[service worker] [db]", err);
      });


      var request;

      if (stuff.method === "getAll") {
        request =
          db.transaction(stuff.info.storeNames)
            .objectStore(stuff.info.objectStore)
            .getAll();
      } else if (stuff.method === "get") {
        request =
          db.transaction(stuff.info.storeNames)
            .objectStore(stuff.info.objectStore)
            .get(stuff.key);
      } else if (stuff.method === "add") {
        request =
          db.transaction(stuff.info.storeNames, "readwrite")
            .objectStore(stuff.info.objectStore)
            .add(stuff.data);
      } else if (stuff.method === "put") {
        request =
          db.transaction(stuff.info.storeNames, "readwrite")
            .objectStore(stuff.info.objectStore)
            .put(stuff.data);
      } else if (stuff.method === "delete") {
        request =
          db.transaction(stuff.info.storeNames, "readwrite")
            .objectStore(stuff.info.objectStore)
            .delete(stuff.key);
      }

      if (request) {
        request.addEventListener("error", err => {
          console.error("[service worker] [db]", err);
        });

        request.addEventListener("success", () => {
          worker.ports.onDbResponse.send({
            "uuid": stuff.info.uuid,
            "data": JSON.stringify(request.result || {})
          });
        });
      }
    });
  });
};

const createObjectStore = (db, name, keyPath) => {
  console.log("[service worker] [db] creating object store");

  db.createObjectStore(
    name, {
      keyPath: keyPath
    }
  );
};


const getRandomInts = (n) => {
  const randInts = new Uint32Array(n);
  crypto.getRandomValues(randInts);
  return Array.from(randInts);
};
