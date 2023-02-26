import { getToken } from '#auth'
import { Octokit } from "octokit";

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  
  if(token) {
      const octokit = new Octokit({ auth: token.accessToken });
      const repoName = event.context.params.repos
    
      const repos = await octokit.request(`GET /orgs/${repoName}/repos`, {
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })
    
      if(repos) {      
        return repos.data
      } else {
        return repos
      }
  } else {
    return null
  }
})