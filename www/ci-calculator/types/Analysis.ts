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
    total_time_ms: number;
    completed: boolean;
    workflows: GithubActionWorkflow[]

    constructor(ci_runs: number, workflows: GithubActionWorkflow[]) {
        this.ci_runs = ci_runs
        this.workflows = workflows
        this.total_time_ms = this.calculate_total_time_ms(workflows)
        this.minutes_on_average = this.calculate_average_workflow_time(this.total_time_ms, workflows.length)
        this.completed = workflows.length > 0;
    }

    calculate_total_time_ms(workflows: GithubActionWorkflow[]): number {
        let total_ms = workflows.reduce((acc, workflow) => acc + workflow.duration_ms, 0)
        if (total_ms > 0) {
            return total_ms
        } else {
            return 0
        }
    }

    calculate_average_workflow_time(total_time_ms: number, number_of_workflows: number): number {
        return total_time_ms / number_of_workflows
    }
}

export {Analysis, GithubActionAnalysis};