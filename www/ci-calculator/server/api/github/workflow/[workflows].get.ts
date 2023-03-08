import { getToken } from '#auth'
import { Octokit } from "octokit";
import {GithubActionWorkflow, Workflow} from "@/types/Workflow";
import {GithubActionAnalysis} from '@/types/Analysis';

async function analyzeWorkflows(workflows: Workflow[]) {
  const new_workflows = workflows.map(workflow => new GithubActionWorkflow(workflow.id.toString(), workflow.created_at, workflow.updated_at, workflow.conclusion))
  return new GithubActionAnalysis(0, new_workflows)
}

export default eventHandler(async (event) => {
  const token = await getToken({ event })
  
  if(token) {
      const octokit = new Octokit({ auth: token.accessToken });
      const query = getQuery(event)
      const repoName = event.context.params ? event.context.params.workflows : null
      const ownerName = query.owner
    
      const workflows = await octokit.request(`GET /repos/{owner}/{repo}/actions/runs`, {
        owner: ownerName,
        repo: repoName,
        per_page: 100,
        headers: {
          'X-GitHub-Api-Version': '2022-11-28'
        }
      })
    
      if(workflows.data.workflow_runs) {
        return analyzeWorkflows(workflows.data.workflow_runs)
      } else {
        return null
      }
  } else {
    return null
  }
})