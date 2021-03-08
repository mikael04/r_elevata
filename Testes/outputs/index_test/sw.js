importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.0.2/workbox-sw.js');

const cacheVersion = "v1";

const {registerRoute} = workbox.routing;
const {NetworkFirst, StaleWhileRevalidate, CacheFirst, } = workbox.strategies;
const {CacheableResponse} = workbox.cacheableResponse;
const {ExpirationPlugin} = workbox.expiration;
const {BroadcastUpdatePlugin} = workbox.broadcastUpdate;


// Cache page navigations (html) with a Network First strategy
registerRoute(
  // Check to see if the request is a navigation to a new page
  ({ request }) =>
    request.mode === 'navigate'||
    request.destination === 'worker',
  // Use a Network First caching strategy
  new NetworkFirst({
    // Put all cached files in a cache named 'pages'
    cacheName: 'pages',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponse({
        statuses: [200],
      }),
      new ExpirationPlugin({
        maxAgeSeconds: 20,
      })
    ],
  }),
);

// Cache CSS, JS, and Web Worker requests with a Stale While Revalidate strategy
registerRoute(
  // Check to see if the request's destination is style for stylesheets, script for JavaScript, or worker for web worker
  ({ request }) =>
    request.destination === 'style' ||
    request.destination === 'script',
  // Use a Stale While Revalidate caching strategy
  new StaleWhileRevalidate({
    // Put all cached files in a cache named 'assets'
    cacheName: 'assets',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponse({
        statuses: [200],
      }),
    ],
  }),
);

// Cache images with a Cache First strategy
registerRoute(
  // Check to see if the request's destination is style for an image
  ({ request }) => request.destination === 'image',
  // Use a Cache First caching strategy
  new CacheFirst({
    // Put all cached files in a cache named 'images'
    cacheName: 'images',
    plugins: [
      // Ensure that only requests that result in a 200 status are cached
      new CacheableResponse({
        statuses: [200],
      }),
      // Don't cache more than 50 items, and expire them after 30 days
      new ExpirationPlugin({
        maxEntries: 50,
        maxAgeSeconds: 60 * 60 * 24 * 30, // 30 Days
      }),
    ],
  }),
);

registerRoute(
  // Adjust this to match your cached data requests:
  new RegExp('/'),
  new StaleWhileRevalidate({
    plugins: [
      new BroadcastUpdatePlugin(),
    ],
  })
);
