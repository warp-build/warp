import { getToken } from '#auth'
import { Octokit } from "octokit";

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  const workflow_repo = event.context.params.workflows
  
  if(workflow_repo && token) {
      const octokit = new Octokit({ auth: token.accessToken });
    
      const repos = await octokit.request('GET /orgs/warp-build/repos', {
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