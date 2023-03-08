import { Workflow, GithubActionWorkflow } from "@/types/Workflow";

interface Analysis {
    repoName: string;
    ci_runs: number;
    minutes_on_average: string;
    completed: boolean;
    workflows: Workflow[]
}

class GithubActionAnalysis implements Analysis {
    repoName: string;
    ci_runs: number;
    minutes_on_average: string;
    completed: boolean;
    workflows: GithubActionWorkflow[]


    constructor(repoName: string, ci_runs: number, workflows: GithubActionWorkflow[]) {
        this.repoName = repoName;
        this.ci_runs = ci_runs
        this.workflows = workflows
        this.minutes_on_average = this.calculate_minutes_on_average()
        this.completed = true;
    }

    calculate_minutes_on_average(): string {
        return "5"
    }
}

export {Analysis, GithubActionAnalysis};