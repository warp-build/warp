interface User {
    repoName: string;
    ci_runs: number;
    completed: boolean;
}

class GithubUser implements User {
    repoName: string;
    ci_runs: number;
    completed: boolean;

    constructor(repoName: string, ci_runs: number) {
        this.repoName = repoName;
        this.ci_runs = ci_runs
        this.completed = true;
    }

}

export {User, GithubUser};