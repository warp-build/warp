import { getToken } from '#auth'
import { Octokit } from "octokit";
import Workflow from "@/types/Workflow";
import Analysis from '~~/types/Analysis';

async function analyzeWorkflows(workflows: Workflow[]) {
  return {
    completed: true
  }
}

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  
  if(token) {
      const octokit = new Octokit({ auth: token.accessToken });
      const query = getQuery(event)
      const repoName = event.context.params.workflows
      const ownerName = query.owner
    
      const workflows = await octokit.request(`GET /repos/${ownerName}/${repoName}/actions/workflows`, {
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })
    
      if(workflows.data.workflows) {      
        return analyzeWorkflows(workflows.data.workflows)
      } else {
        return null
      }
  } else {
    return null
  }
})