import { getToken } from '#auth'
import { Octokit, App } from "octokit";
export default eventHandler(async (event) => {
  const token = await getToken({ event, raw: true })
  
  if(token) {
      const octokit = new Octokit({ auth: token });
    
      const orgs = await octokit.request('GET /organizations', {
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })
    
      return orgs
  } else {
    return null
  }
})