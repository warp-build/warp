import { defineConfig } from "cf/config";

export default defineConfig({
  worker: {
    "name": "warp-leostera-dev",
    "compatibilityDate": "2026-10-14",
    "observability": {
      "enabled": true
    },
    "assets": {
      "notFoundHandling": "single-page-application"
    }
  }
});
