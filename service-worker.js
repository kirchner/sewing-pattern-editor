// Files to cache
var cacheName = 'sewing-pattern-editor--v6';
var appShellFiles = [
  '/app.html',
  '/static/elm.js',
  '/static/manifest.webmanifest',
  '/static/main.css',
  '/static/icon-32.png',
  '/static/icon-64.png',
  '/static/icon-128.png',
  '/static/icon-256.png',
  '/static/icon-512.png',
  '/static/rubik-v9-latin-300.woff',
  '/static/rubik-v9-latin-300.woff2',
  '/static/mansalva-v1-latin-regular.woff',
  '/static/mansalva-v1-latin-regular.woff2',
  '/static/css/fontawesome.min.css',
  '/static/css/solid.min.css',
  '/static/webfonts/fa-solid-900.eot',
  '/static/webfonts/fa-solid-900.svg',
  '/static/webfonts/fa-solid-900.ttf',
  '/static/webfonts/fa-solid-900.woff',
  '/static/webfonts/fa-solid-900.woff2'
];

// Installing Service Worker
self.addEventListener('install', function(e) {
  console.log('[Service Worker] Install');
  e.waitUntil(
    caches.open(cacheName).then(function(cache) {
      console.log('[Service Worker] Caching all: app shell and content');
      return cache.addAll(appShellFiles);
    })
  );
});

// Activating Service Worker
self.addEventListener('activate', function(event) {
  console.log('[Service Worker] Activate');

  var cacheWhitelist = [cacheName];

  event.waitUntil(
    caches.keys().then(function(cacheNames) {
      return Promise.all(
        cacheNames.map(function(cacheName) {
          if (cacheWhitelist.indexOf(cacheName) === -1) {
            console.log('[Service Worker] Deleting cache:', cacheName);
            return caches.delete(cacheName);
          }
        })
      );
    })
  );
});

// Fetching content using Service Worker
self.addEventListener('fetch', function(e) {
  e.respondWith(
    caches.match(e.request).then(function(r) {
      console.log('[Service Worker] Fetching resource: '+e.request.url);
      return r || fetch(e.request).then(function(response) {
        return caches.open(cacheName).then(function(cache) {
          console.log('[Service Worker] Caching new resource: ' + e.request.url);
          cache.put(e.request, response.clone());
          return response;
        });
      });
    })
  );
});
