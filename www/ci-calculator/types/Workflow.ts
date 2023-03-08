
enum WorkflowConclusion {
    Failure = "FAILURE",
    Success = "SUCCESS"
}

interface Workflow {
    conclusion: WorkflowConclusion;
    created_at: string;
    updated_at: string
    id: string;
}

class GithubActionWorkflow implements Workflow {
    conclusion: WorkflowConclusion;
    created_at: string
    updated_at: string
    id: string;
    duration_ms: number

    public constructor(id: string, created_at: string, updated_at:string, conclusion: string) {
        this.id = id;
        this.created_at = created_at
        this.updated_at = updated_at
        this.duration_ms = this.calculate_duration(created_at, updated_at)

        switch (conclusion.toUpperCase()) {
            case 'SUCCESS':
                this.conclusion = WorkflowConclusion.Success
                break;
            default:
                this.conclusion = WorkflowConclusion.Failure
        }
    }

    calculate_duration(created_at: string, updated_at: string): number {
        let start = Date.parse(created_at)
        let end = Date.parse(updated_at)
        return end - start
    }

}

export {Workflow, GithubActionWorkflow};