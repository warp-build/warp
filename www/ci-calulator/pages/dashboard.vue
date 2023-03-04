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
    <div class="mx-auto max-w-7xl py-6 sm:px-6 lg:px-8">
      <!-- Replace with your content -->
      <div class="mx-auto max-w-2xl text-center">
      <h2 class="text-3xl font-bold tracking-tight text-gray-900 sm:text-4xl">Github Actions Analysis</h2>
      <p class="mt-2 text-lg leading-8 text-gray-600">Aute magna irure deserunt veniam aliqua magna enim voluptate.</p>
    </div>
      <div class="mx-auto mt-16 max-w-xl sm:mt-20">
        <Dropdown @selectedOption="fetchRepos" :organizations="propsToPass()"/>
        <DropdownRepos :repositories="repositories"/>
        <div class="mt-10">
          <button type="submit" class="block w-full rounded-md bg-indigo-600 px-3.5 py-2.5 text-center text-sm font-semibold text-white shadow-sm hover:bg-indigo-500 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-indigo-600">Let's go!</button>
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

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: organizations } = await useFetch('/api/github/organizations', { headers });


async function fetchRepos(repoName: string) {
  repositories.value = await useFetch(`/api/github/${repoName}`, { headers }).data.value;
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

function reposToPass() {
  return []
}

</script>

<style scoped>

</style>