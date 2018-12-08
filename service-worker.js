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


self.addEventListener("install", event => {
  self.skipWaiting();
});

self.addEventListener("activate", event => {
  event.waitUntil(dbPromise);
});


self.addEventListener("fetch", event => {
  var method = event.request.method;
  var url = new URL(event.request.url);
  var segments = url.pathname.split("/");
  segments.shift();

  if (method === "GET" && segments.length === 1 && segments[0] === "patterns") {
    event.respondWith(
      getPatterns()
    );
  } else if (method === "GET" && segments.length === 2 && segments[0] === "patterns") {
    event.respondWith(
      getPattern(segments[1])
    );
  } else if (method === "POST" && segments.length === 1 && segments[0] === "patterns") {
    event.respondWith(
      event.request.json().then(storePattern)
    );
  } else if (method === "PUT" && segments.length === 1 && segments[0] === "patterns") {
    event.respondWith(
      event.request.json().then(updatePattern)
    );
  }
});


//----

const dbPromise = idb.open("sewing-pattern-editor", 1, upgradeDB => {
  upgradeDB.createObjectStore("patterns", { keyPath: "slug" });
});

const getPatterns = () =>
  dbPromise.then(db => {
    return db.transaction("patterns")
      .objectStore("patterns")
      .getAll();
  })
  .then(patterns => {
    return new Response(
      new Blob(
        [JSON.stringify(patterns)],
        { type: "application/json" }
      ),
      {
        status: 200,
        statusText: "OK"
      }
    );
  });

const getPattern = slug =>
  dbPromise.then(db => {
    return db.transaction("patterns")
      .objectStore("patterns")
      .get(slug);
  })
  .then(pattern => {
    return new Response(
      new Blob(
        [JSON.stringify(pattern)],
        { type: "application/json" }
      ),
      {
        status: 200,
        statusText: "OK"
      }
    )
  });

const storePattern = data =>
  dbPromise.then(db => {
    return db.transaction("patterns", "readwrite")
      .objectStore("patterns")
      .add(data);
  })
  .then(id => {
    return new Response(
      new Blob(
        [JSON.stringify({ id: id })],
        { type: "application/json" }
      ),
      {
        status: 201,
        statusText: "Created"
      }
    );
  })
  .catch(error => {
    return new Response(
      new Blob(
        [JSON.stringify({ error: error.message })],
        { type: "application/json" }
      ),
      {
        status: 403,
        statusText: "Forbidden"
      }
    );
  });

const updatePattern = data =>
  dbPromise.then(db => {
    const tx = db.transaction("patterns", "readwrite");
    tx.objectStore("patterns")
      .put(data);
    return tx.complete;
  })
  .then(id => {
    return new Response(
      new Blob(
        [JSON.stringify({ id: id })],
        { type: "application/json" }
      ),
      {
        status: 200,
        statusText: "OK"
      }
    );
  })
  .catch(error => {
    console.log(error);
    return new Response(
      new Blob(
        [JSON.stringify({ error: error.message })],
        { type: "application/json" }
      ),
      {
        status: 403,
        statusText: "Forbidden"
      }
    );
  });
