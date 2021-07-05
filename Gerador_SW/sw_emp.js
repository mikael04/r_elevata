const cssVersion = '0'
const jsVersion = '0'
const htmlsVersion = '0'
const auxVersion = '0'
const cacheVersion = '0'

const debug = true
if(debug)
  console.log("cssVersion = ", cssVersion);
if(debug)
  console.log("jsVersion = ", jsVersion);
if(debug)
  console.log("htmlsVersion = ", htmlsVersion);
if(debug)
  console.log("auxVersion = ", auxVersion);
if(debug)
  console.log("CacheVersion = ", cacheVersion);

importScripts('/js/workbox-sw.js');

self.addEventListener('install', function(event) {
  if(debug)
    console.log("Instalando novo sw");
  // The promise that skipWaiting() returns can be safely ignored.
  //event.waitUntil(caches.open(cacheName).then((cache) => cache.addAll(urls)));
  self.skipWaiting();
  workbox.core.clientsClaim();
});

self.addEventListener('activate', function(event) {
  if(debug)
    console.log("Ativando novo sw");
  clearOldCaches(event);
});


workbox.precaching.precacheAndRoute([
  //manifest
  {url: '/manifest.json', revision: cacheVersion },
  //css
  // {url: '/css/fontawesome.min.css', revision: auxVersion },
  {url: '/css/bootstrap.min.css', revision: cacheVersion },
  {url: '/css/main.css', revision: cssVersion },
  {url: '/css/material-modal.css', revision: cacheVersion },
  {url: '/css/navbar.css', revision: cssVersion },
  {url: '/css/topbar.css', revision: cssVersion },
  /////
  //js
  {url: '/js/auth.js', revision: jsVersion },
  {url: '/js/auth_login_date_checker.js', revision: jsVersion },
  //Lib para ler dados locais (de empresa, pelo usuário logado)
  {url: '/js/localforage.min.js', revision: cacheVersion },
  {url: '/js/index_date_checker.js', revision: jsVersion },
  {url: '/js/jquery-3.6.0.min.js', revision: cacheVersion },
  {url: '/js/main.js', revision: jsVersion },
  {url: '/js/material-modal.js', revision: cacheVersion },
  // Arquivo de sw
  {url: '/js/workbox-sw.js', revision: cacheVersion },
  //js teste, não vai pra produção
  {url: '/tests/auth.js', revision: jsVersion },
  {url: '/tests/auth_login_date_checker.js', revision: jsVersion },
  //{url: '/.js', revision: null }, bootstrap
  /////
  //htmls
  //Página inicial (login)
  {url: '/index.html', revision: htmlsVersion },
  //Página inicial (visão inicial/geral)
  {url: '/main.html', revision: htmlsVersion },
  //imagens
  {url: '/css/icons/axeler_192x192.png', revision: cacheVersion },
  {url: '/css/icons/axeler_512x512.png', revision: cacheVersion },
  {url: '/favicon.ico', revision: cacheVersion },
  {url: '/imgs/graficos_3.png', revision: cacheVersion },
  {url: '/imgs/lista_wide.png', revision: cacheVersion },
  //outros
  //{url: '/sync_time/update_time_app.txt', revision: updateTimeVersion },
  //{url: '/sync_time/update_time_bd.txt', revision: updateTimeVersion }
  // ... other entries ...
]);



const { offlineFallback } = workbox.recipes;
const { registerRoute } = workbox.routing;
const { NetworkFirst, StaleWhileRevalidate, CacheFirst, NetworkOnly} = workbox.strategies;
const { CacheableResponse } = workbox.cacheableResponse;
const { setDefaultHandler } = workbox.routing;

// Adicionando uma receita de modo offline;
offlineFallback();
console.log("Fallback ativado /offline");

// Clear old Dash caches
var clearOldCaches = function (event)
{
    event.waitUntil(
        caches.keys().then(function (cacheNames) {
            let validCacheSet = new Set(Object.values(workbox.core.cacheNames));
            return Promise.all(
                cacheNames
                .filter(function (cacheName) {
                    return !validCacheSet.has(cacheName);
                })
                .map(function (cacheName) {
                    return caches.delete(cacheName);
                })
            );
        })
    );
};
