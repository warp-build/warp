import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'

import { cloudflare } from '@cloudflare/vite-plugin'

export default defineConfig(({ command }) => {
  if (command === 'build') {
    process.env.NODE_ENV = 'production'
  }

  return {
    plugins: [react(), cloudflare()],
  }
})
