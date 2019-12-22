// Files to cache
var cacheName = 'sewing-pattern-editor--v1';
var appShellFiles = [
  './index.html',
  './assets/icon-32.png',
  './assets/icon-64.png',
  './assets/icon-128.png',
  './assets/icon-256.png',
  './assets/icon-512.png',
  './assets/rubik-v9-latin-300.woff',
  './assets/rubik-v9-latin-300.woff2',
  './assets/mansalva-v1-latin-regular.woff',
  './assets/mansalva-v1-latin-regular.woff2',
  './main.css',
  './index.js',
  './manifest.webmanifest',
  '@fortawesome/fontawesome-free/css/all.css'
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
