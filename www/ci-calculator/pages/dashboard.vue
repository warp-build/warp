<template>
<!--
  This example requires updating your template:

  ```
  <html class="h-full bg-gray-100">
  <body class="h-full">
  ```
-->
<div class="min-h-full">
  <Navbar/>
  <main>
    <div class="mx-auto mt-12 max-w-7xl py-6 sm:px-6 lg:px-8">
      <!-- Replace with your content -->
      <div class="mx-auto max-w-2xl text-center">
      <h2 class="text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl">Github Actions Analysis</h2>
      <p class="mt-2 text-lg leading-8 text-gray-600">Create a new analysis by selecting an organization and an existing Github repository.</p>
    </div>
      <div :class="{'animate-pulse': loading}" class="mx-auto mt-12 max-w-xl sm:mt-12">
        <Dropdown @selectedOption="fetchRepos" :organizations="propsToPass()"/>
        <DropdownRepos :disabled="loading" @selectedRepo="repoSelect" :repositories="repositories"/>
      </div>
      <Stats :analysis="analysis" v-if="showStats"/>
      <!-- /End replace -->
    </div>
  </main>

</div>

</template>

<script setup lang="ts">
import Repository from "@/types/Repository";
import Organization from "@/types/Organization";
import {GithubActionAnalysis} from "@/types/Analysis";

const { status, data } = useSession()
const repositories = ref([])
const loading = ref(false)
const analysis = reactive(new GithubActionAnalysis(0, []))

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: organizations } = await useFetch('/api/github/organizations', { headers });

const showStats = computed(() => {
    return analysis.completed
})

async function fetchRepos(repoName: string) {
  loading.value = !loading.value
  const repos = await useFetch(`/api/github/${repoName}`, { headers }) 

  if (repos.data) {
    repositories.value = repos.data.value;
  }

  loading.value = !loading.value
}
async function runQuery(owner: string, repoName: string) {
  loading.value = !loading.value
  const analysisResult = await useFetch(`/api/github/workflow/${repoName}?owner=${owner}`, { headers }) 

  if (analysisResult.data.value) {
    Object.assign(analysis, analysisResult.data.value)
  }

  loading.value = !loading.value
}

async function repoSelect(repo: Repository) {
  const RepoInfo = repo.full_name.split("/")
  const owner = RepoInfo[0]
  const repoName = RepoInfo[1]
  runQuery(owner, repoName)
}

function propsToPass() {

  const user_added = organizations.value.filter((x: Organization) => x.login === data.value?.user?.login)
  if (user_added.length === 0) {
    let user_org = {
    login: data.value?.user?.login,
    avatar_url: data.value?.user?.image,
    node_id: data.value?.user?.email,
    selected: false,
  }

    organizations.value.push(user_org)
  }

  if (organizations) {
    return organizations.value
  } else {
    return []
  }
}


</script>

<style scoped>


</style>