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
  <header class="bg-white shadow">
    <div class="mx-auto max-w-7xl py-6 px-4 sm:px-6 lg:px-8">
      <h1 class="text-3xl font-bold tracking-tight text-gray-900">New Github Actions Analysis</h1>
    </div>
  </header>
  <main>
    <div class="mx-auto max-w-7xl py-6 sm:px-6 lg:px-8">
      <!-- Replace with your content -->
      <Dropdown :organizations="propsToPass()"/>
      <DropdownRepos :repositories="reposToPass()" :disabled="true"/>
      <!-- /End replace -->
    </div>
  </main>

  <!-- <p>{{ this.$auth.strategy.token.get() }}</p> -->
</div>

</template>

<script setup lang="ts">
const { status, data } = useSession()

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: organizations } = await useFetch('/api/github/organizations', { headers });
// const { data: repositories } = await useFetch('/api/github_repos', { headers });


function propsToPass() {

  const user_added = organizations.value.filter((x) => x.login === data.value?.user?.email)
  if (user_added.length === 0) {
    let user_org = {
    login: data.value?.user?.email,
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