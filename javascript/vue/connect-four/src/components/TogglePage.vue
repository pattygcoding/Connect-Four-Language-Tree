<template>
    <div class="hello-world">
        <h1>{{ message }}</h1>
    </div>
</template>

<script>
export default {
    name: 'TogglePage',
    data() {
        return {
            message: 'Hello, World!'
        };
    }
};
</script>

<style scoped>
.hello-world {
    text-align: center;
    margin-top: 20px;
}
</style>