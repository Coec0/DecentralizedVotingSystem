<template>
  <div class="nav">
    <img id="logo" src="logo.png">
    <NavButton text="Home" to="/"></NavButton>
    <NavButton text="Admin" to="/admin"></NavButton>
  </div>
</template>

<script>
import NavButton from "@/components/Navigation/NavButton.vue";
export default {
  name: "Navigation",
  components: {
    NavButton
  }
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.nav {
  width: 100%;
  display: flex;
  align-items: center; 
  background-color: #42b983;
}
#logo {
  margin: 15px;
}
</style>
