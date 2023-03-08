<template>
    <div class="bg-white py-24 sm:py-32">
    <div class="mx-auto max-w-7xl px-6 lg:px-8">
        <dl class="grid grid-cols-1 gap-y-16 gap-x-8 text-center lg:grid-cols-3">
        <div class="mx-auto flex max-w-xs flex-col gap-y-4">
            <dt class="text-base leading-7 text-gray-600">Total time spent waiting (min)</dt>
            <dd class="order-first text-3xl font-semibold tracking-tight text-gray-900 sm:text-5xl">{{ millisToMinutesAndSeconds(props.analysis?.total_time_ms) }}</dd>
        </div>

        <div class="mx-auto flex max-w-xs flex-col gap-y-4">
            <dt class="text-base leading-7 text-gray-600">Number of Workflows</dt>
            <dd class="order-first text-3xl font-semibold tracking-tight text-gray-900 sm:text-5xl">{{ props.analysis?.workflows.length }}</dd>
        </div>

        <div class="mx-auto flex max-w-xs flex-col gap-y-4">
            <dt class="text-base leading-7 text-gray-600">Duration of average Workflow (min)</dt>
            <dd class="order-first text-3xl font-semibold tracking-tight text-gray-900 sm:text-5xl">{{ millisToMinutesAndSeconds(props.analysis?.minutes_on_average) }}</dd>
        </div>
        </dl>
    </div>
    </div>
</template>

<script setup lang="ts">
import {GithubActionAnalysis} from '@/types/Analysis';

const props = defineProps({
    analysis: {
        type: Object as ()=> GithubActionAnalysis
    }
})

function millisToMinutesAndSeconds(millis: number) {
  var minutes = Math.floor(millis / 60000);
  var seconds = ((millis % 60000) / 1000).toFixed(0);
  return minutes + ":" + (seconds < 10 ? '0' : '') + seconds;

//   const date = new Date(millis)
//   return `${date.getMinutes()}:${date.getSeconds()}`
}


</script>

<style scoped>

</style>