
// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
  modules: [
      '@nuxtjs/tailwindcss',
      '@sidebase/nuxt-auth'
    ],
    auth: {
      origin: process.env.ORIGIN,
      enableGlobalAppMiddleware: true
  },
  routeRules: {
    '/api/**': { cors: true },
  }
})
