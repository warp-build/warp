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
      <!-- /End replace -->
    </div>
  </main>

  <!-- <p>{{ this.$auth.strategy.token.get() }}</p> -->
</div>

</template>

<script setup lang="ts">
const { status, data } = useSession()

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: organizations } = await useFetch('/api/github_organizations', { headers });
// const { data: repositories } = await useFetch('/api/github_repos', { headers });


// const [{ data: repositories }, { data: repos }] = await Promise.all([
//       useFetch(`https://api.github.com/orgs/nuxt`),
//       useFetch(`https://api.github.com/orgs/nuxt/repos`)
//     ])

function propsToPass() {
  let props: any[] = []

  let user_org = {
    login: data.value?.user?.email,
    avatar_url: data.value?.user?.image,
    node_id: data.value?.user?.email,
    selected: false,
  }

  props.push(user_org)

  if (organizations) {
    organizations.value.push(user_org)
    return organizations.value
  } else {
    return props
  }
}


</script>

<style scoped>

</style>