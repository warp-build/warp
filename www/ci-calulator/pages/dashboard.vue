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
      <h1 class="text-3xl font-bold tracking-tight text-gray-900">Dashboard</h1>
    </div>
  </header>
  <main>
    <div class="mx-auto max-w-7xl py-6 sm:px-6 lg:px-8">
      <!-- Replace with your content -->
      <div class="px-4 py-6 sm:px-0">
        <div class="h-96 rounded-lg border-4 border-dashed border-gray-200"></div>
      </div>
      <!-- /End replace -->
    </div>
  </main>

  <!-- <p>{{ this.$auth.strategy.token.get() }}</p> -->
</div>

</template>

<script setup lang="ts">
const { status, data, signIn, signOut } = useSession()

const headers = useRequestHeaders(['cookie']) as HeadersInit;
const { data: token } = await useFetch('/api/token', { headers });


// const [{ data: organization }, { data: repos }] = await Promise.all([
//       useFetch(`https://api.github.com/orgs/nuxt`),
//       useFetch(`https://api.github.com/orgs/nuxt/repos`)
//     ])

</script>

<style scoped>

</style>