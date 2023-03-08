import { Workflow, GithubActionWorkflow } from "@/types/Workflow";

interface Analysis {
    ci_runs: number;
    minutes_on_average: number;
    completed: boolean;
    workflows: Workflow[]
}

class GithubActionAnalysis implements Analysis {
    ci_runs: number;
    minutes_on_average: number;
    completed: boolean;
    workflows: GithubActionWorkflow[]

    constructor(ci_runs: number, workflows: GithubActionWorkflow[]) {
        this.ci_runs = ci_runs
        this.workflows = workflows
        this.minutes_on_average = this.calculate_minutes_on_average(workflows)
        this.completed = workflows.length > 0;
    }

    calculate_minutes_on_average(workflows: GithubActionWorkflow[]): number {
        let total_ms = workflows.reduce((acc, workflow) => acc + workflow.duration_ms, 0)
        if (total_ms > 0) {
            return total_ms / 1000 / 60 / workflows.length
        } else {
            return 0
        }
    }
}

export {Analysis, GithubActionAnalysis};