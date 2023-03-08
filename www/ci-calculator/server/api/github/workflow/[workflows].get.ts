import { getToken } from '#auth'
import { Octokit } from "octokit";
import {GithubActionWorkflow, createWorkflows} from "@/types/Workflow";
import {GithubActionAnalysis} from '@/types/Analysis';

async function analyzeWorkflows(workflows: GithubActionWorkflow[]) {
  return new GithubActionAnalysis({completed: true, })
}

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  
  if(token) {
      const octokit = new Octokit({ auth: token.accessToken });
      const query = getQuery(event)
      const repoName = event.context.params ? event.context.params.workflows : null
      const ownerName = query.owner
    
      const workflows = await octokit.request(`GET /repos/${ownerName}/${repoName}/actions/workflows`, {
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })
    
      if(workflows.data.workflows) {
        return analyzeWorkflows(createWorkflows(workflows.data.workflows))
      } else {
        return null
      }
  } else {
    return null
  }
})