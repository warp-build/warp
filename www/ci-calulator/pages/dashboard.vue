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
      <div class="mx-auto mt-12 max-w-xl sm:mt-12">
        <Dropdown :class="{'animate-pulse': loading}" @selectedOption="fetchRepos" :organizations="propsToPass()"/>
        <DropdownRepos :repositories="repositories"/>
        <div class="mt-6">
          <button @click="runQuery" type="submit" class="block w-full rounded-md bg-indigo-600 px-3.5 py-2.5 text-center text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600">
            <svg v-show="loading" aria-hidden="true" role="status" class="inline w-4 h-4 mr-3 text-white animate-spin" viewBox="0 0 100 101" fill="none" xmlns="http://www.w3.org/2000/svg">
            <path v-show="loading" d="M100 50.5908C100 78.2051 77.6142 100.591 50 100.591C22.3858 100.591 0 78.2051 0 50.5908C0 22.9766 22.3858 0.59082 50 0.59082C77.6142 0.59082 100 22.9766 100 50.5908ZM9.08144 50.5908C9.08144 73.1895 27.4013 91.5094 50 91.5094C72.5987 91.5094 90.9186 73.1895 90.9186 50.5908C90.9186 27.9921 72.5987 9.67226 50 9.67226C27.4013 9.67226 9.08144 27.9921 9.08144 50.5908Z" fill="#E5E7EB"/>
            <path v-show="loading" d="M93.9676 39.0409C96.393 38.4038 97.8624 35.9116 97.0079 33.5539C95.2932 28.8227 92.871 24.3692 89.8167 20.348C85.8452 15.1192 80.8826 10.7238 75.2124 7.41289C69.5422 4.10194 63.2754 1.94025 56.7698 1.05124C51.7666 0.367541 46.6976 0.446843 41.7345 1.27873C39.2613 1.69328 37.813 4.19778 38.4501 6.62326C39.0873 9.04874 41.5694 10.4717 44.0505 10.1071C47.8511 9.54855 51.7191 9.52689 55.5402 10.0491C60.8642 10.7766 65.9928 12.5457 70.6331 15.2552C75.2735 17.9648 79.3347 21.5619 82.5849 25.841C84.9175 28.9121 86.7997 32.2913 88.1811 35.8758C89.083 38.2158 91.5421 39.6781 93.9676 39.0409Z" fill="currentColor"/>
            </svg>
            {{submitButtonText}}
          </button>
        </div>
      </div>
      <!-- /End replace -->
    </div>
  </main>

</div>

</template>

<script setup lang="ts">
const { status, data } = useSession()
const repositories = ref([])
const loading = ref(false)

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: organizations } = await useFetch('/api/github/organizations', { headers });

const submitButtonText = computed(() => {
      return (loading.value) ? "Loading..." : "Let's go!";
  });

async function fetchRepos(repoName: string) {
  loading.value = !loading.value
  const repos = await useFetch(`/api/github/${repoName}`, { headers }) 

  if (repos.data) {
    repositories.value = repos.data.value;
  }

  loading.value = !loading.value
}
async function runQuery() {
  const repos = await useFetch(`/api/github/$`, { headers }) 

  if (repos.data) {
    repositories.value = repos.data.value;
  }
}


function propsToPass() {

  const user_added = organizations.value.filter((x) => x.login === data.value?.user?.login)
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