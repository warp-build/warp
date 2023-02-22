<template>
    <div>
    <button type="submit" @click="signIn()" class="group relative flex w-full justify-center rounded-md border border-transparent bg-indigo-600 py-2 px-4 text-sm font-medium text-white hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2">
        <span class="absolute inset-y-0 left-0 flex items-center pl-3">
            <img src="/github-mark-white.png" class="h-5 w-5 text-indigo-500 group-hover:text-indigo-400">
        </span>
        Login with Github
        </button>
    </div>
</template>

<script setup lang="ts">
const { status, data, signIn, signOut } = useSession()

</script>

<style scoped>

</style>