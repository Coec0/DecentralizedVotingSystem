<template>
  <div class="button">
    <router-link v-bind:to="to" tag="div">{{ text }}</router-link>
  </div>
</template>

<script>
export default {
  name: "NavButton",
  props: {
    text: String,
    to: String
  }
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.button {
  width: 100px;
  height: 50px;
  background-color: #42b983;
  -webkit-transition-duration: 0.4s; /* Safari */
  transition-duration: 0.4s;
}
.button:hover {
   color: white;
   background-color: #42b983;
   cursor: default;
}
</style>
