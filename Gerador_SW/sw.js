const cssVersion = '0'
const jsVersion = '0'
const htmlsVersion = '0'
const dashsVersion = '05072021_083344'
const listasVersion = '05072021_083344'
const auxVersion = '0'
const cacheVersion = '0'
const updateTimeVersion = '05072021_083344'

const debug = true

if(debug)
  console.log("cssVersion = ", cssVersion);
if(debug)
  console.log("jsVersion = ", jsVersion);
if(debug)
  console.log("htmlsVersion = ", htmlsVersion);
if(debug)
  console.log("dashsVersion = ", dashsVersion);
if(debug)
  console.log("listasVersion = ", listasVersion);
if(debug)
  console.log("auxVersion = ", auxVersion);
if(debug)
  console.log("CacheVersion = ", cacheVersion);
if(debug)
  console.log("updateTimeVersion = ", updateTimeVersion);

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
});


workbox.precaching.precacheAndRoute([
  //manifest
  {url: '/manifest.json', revision: cacheVersion },
  //css
  // {url: '/css/fontawesome.min.css', revision: cacheVersion },
  {url: '/css/bootstrap.min.css', revision: cacheVersion },
  {url: '/css/graficos.css', revision: cssVersion },
  {url: '/css/listas.css', revision: cssVersion },
  {url: '/css/listas_iframe.css', revision: cssVersion },
  {url: '/css/main.css', revision: cssVersion },
  {url: '/css/material-modal.css', revision: cacheVersion },
  {url: '/css/navbar.css', revision: cssVersion },
  {url: '/css/topbar.css', revision: cssVersion },
  /////
  //js
  {url: '/js/auth.js', revision: jsVersion },
  {url: '/js/auth_login_date_checker.js', revision: jsVersion },
  {url: '/js/graficos.js', revision: jsVersion },
  //Lib para ler dados locais (de empresa, pelo usuário logado)
  {url: '/js/localforage.min.js', revision: cacheVersion },
  {url: '/js/index_date_checker.js', revision: jsVersion },
  {url: '/js/jquery-3.6.0.min.js', revision: cacheVersion },
  {url: '/js/listas.js', revision: jsVersion },
  //Lib para selecionar campos (busca)
  {url: '/js/lista_b24_d5.js', revision: jsVersion },
  {url: '/js/lista_b245_d7.js', revision: jsVersion },
  //Lib para manipular data
  {url: '/js/luxon.min.js', revision: cacheVersion },
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
  //Dashs
  {url: '/graficos.html', revision: htmlsVersion },
  //Listas
  {url: '/listas.html', revision: htmlsVersion },
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

importScripts('/js/localforage.min.js');

localforage.getItem('empresa').then(
  function(readValue){
    if(debug)
      console.log('Valor lido = ', readValue)
    empresaID = readValue;
    if(readValue != null){
      workbox.precaching.precacheAndRoute([
        // Dashs
        {url: (String('/src/Dashs/Geral_') + String(readValue) + String('.html')), revision: dashsVersion },
        {url: (String('/src/Dashs/Marcas_') + String(readValue) + String('.html')), revision: dashsVersion },
        {url: (String('/src/Dashs/Negocios_') + String(readValue) + String('.html')), revision: dashsVersion },
        {url: (String('/src/Dashs/Propostas_') + String(readValue) + String('.html')), revision: dashsVersion },
        {url: (String('/src/Dashs/Visitas_Clientes_') + String(readValue) + String('.html')), revision: dashsVersion },


        // Listas
        {url: (String('/src/Listas/avaliacoes/htmls_final/avaliacoes_') + String(readValue) + String('.html')), revision: listasVersion },
        {url: (String('/src/Listas/negocios/htmls_final/negocios_') + String(readValue) + String('.html')), revision: listasVersion },
        {url: (String('/src/Listas/propostas/htmls_final/propostas_') + String(readValue) + String('.html')), revision: listasVersion },
        {url: (String('/src/Listas/visitas/htmls_final/visitas_') + String(readValue) + String('.html')), revision: listasVersion },
      ]);
    }
  }

)

const { offlineFallback } = workbox.recipes;
const { registerRoute } = workbox.routing;
const { NetworkFirst, StaleWhileRevalidate, CacheFirst, NetworkOnly} = workbox.strategies;
const { CacheableResponse } = workbox.cacheableResponse;
const { setDefaultHandler } = workbox.routing;

// Adicionando uma receita de modo offline;
offlineFallback();
if(debug)
  console.log("Fallback ativado /offline");

// Essa rota de network first será usada para atualizar os arquivos responsáveis pelos tempos de sincronia
registerRoute(
  ({
    url
  }) => url.origin === self.location.origin &&
  url.pathname.startsWith('/api/Login/') || url.pathname.startsWith('https://homolog2.api.elevor.com.br/api/Login/'),
  new NetworkOnly({
    cacheName: 'API login'
  })
);

registerRoute(
  ({
    url
  }) => url.origin === self.location.origin &&
  url.pathname.startsWith('/sync_time/') ,
  new NetworkFirst({
    cacheName: 'sync_time'
  })
);

// Register 'default'
defaultStrategy = new CacheFirst ({
  cacheName: "outros",
  plugins: [
      new workbox.expiration.ExpirationPlugin({
          maxEntries: 128,
          maxAgeSeconds: 7 * 24 * 60 * 60, // 1 week
          purgeOnQuotaError: true, // Opt-in to automatic cleanup
      }),
      new workbox.cacheableResponse.CacheableResponse({
          statuses: [0, 200] // for opague requests
      }),
  ],
});
workbox.routing.setDefaultHandler(
  (args) => {
      if (args.event.request.method === 'GET') {
          return defaultStrategy.handle(args); // use default strategy
      }
      return fetch(args.event.request);
  }
);
