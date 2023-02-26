import { getToken } from '#auth'
import { Octokit } from "octokit";

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  
  if(token) {
      const octokit = new Octokit({ auth: token.accessToken });
    
      const orgs = await octokit.request('GET /users/enkronan/orgs', {
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })

      if (orgs) {
        return orgs.data
      } else {
        return orgs
      }
    
  } else {
    return null
  }
})