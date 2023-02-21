export default defineNuxtPlugin(() => {
    return {
      provide: {
        login: (msg: string) => `Hello ${msg}!`
      }
    }
  })