// file: ~/server/api/auth/[...].ts
import { NuxtAuthHandler } from '#auth'
import GithubProvider from 'next-auth/providers/github'
export default NuxtAuthHandler({
    secret: process.env.AUTH_SECRET,
    pages: {
        // Change the default behavior to use `/login` as the path for the sign-in page
        signIn: '/'
      },
    callbacks: {
        // async jwt({ token, account, profile }) {
        //   // Persist the OAuth access_token and or the user id to the token right after signin
        //   if (account) {
        //     token.accessToken = account.access_token
        //   }

        //   if (profile) {
        //     token.login = profile.login
        //   }
        //   return token
        // },

        jwt: async ({token, account, profile, user}) => {
          const isSignIn = user ? true : false;
          if (isSignIn && account) {
            token.accessToken = account.access_token
          }

          if (profile) {
            token.login = profile.login
          }

          return Promise.resolve(token)
        },
      //   async session({ session, token }) {
      //       // Send properties to the client, like an access_token and user id from a provider.
      //       if (session.user) {
      //         session.user.login = token.login
      //       }
            
      //       return session
      //     }
      // }
      session: async ({session, token}) => {
        if (session.user){
          session.user.login = token.login
        }

        return Promise.resolve(session);
      }
    }, 
    providers: [
      // @ts-expect-error You need to use .default here for it to work during SSR. May be fixed via Vite at some point
      GithubProvider.default({
          clientId: process.env.GITHUB_CLIENT_ID,
          clientSecret: process.env.GITHUB_CLIENT_SECRET,  
      })
  ]
})