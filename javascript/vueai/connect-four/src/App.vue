<!-- src/App.vue -->
<template>
  <div id="app">
    <ConnectFour />
  </div>
</template>

<script>
import ConnectFour from './components/ConnectFour.vue'

export default {
  name: 'App',
  components: {
    ConnectFour
  }
}
</script>
