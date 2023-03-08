
enum WorkflowConclusion {
    Failure = "FAILURE",
    Success = "SUCCESS"
}

interface Workflow {
    conclusion: WorkflowConclusion;
    duration_ms: number;
    id: string;
}

class GithubActionWorkflow implements Workflow {
    conclusion: WorkflowConclusion;
    duration_ms: number;
    id: string;

    constructor(id: string, duration_ms: number, conclusion: WorkflowConclusion) {
        this.id = id;
        this.duration_ms = duration_ms
        this.conclusion = conclusion
    }

}

function createWorkflows(workflows: Workflow[]): GithubActionWorkflow[] {
    return []
}

export {Workflow, GithubActionWorkflow, createWorkflows};