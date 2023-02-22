// file: ~/server/api/auth/[...].ts
import { NuxtAuthHandler } from '#auth'
import GithubProvider from 'next-auth/providers/github'
export default NuxtAuthHandler({
    secret: process.env.NUXT_SECRET,
    providers: [
        // @ts-expect-error You need to use .default here for it to work during SSR. May be fixed via Vite at some point
        GithubProvider.default({
            clientId: process.env.GITHUB_CLIENT_ID,
            clientSecret: process.env.GITHUB_CLIENT_SECRET
        })
    ]
})