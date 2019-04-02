<template>
  <div v-if="show">
    <p>{{text}}</p>
    <img v-bind:src="link"></img>
  </div>
</template>

<script>
export default {
  name: "BlinkingImage",
  data(){
      return {
          show: true
      }
  },
  created(){
      setInterval(()=>{
         // this.show=!this.show;
      }, 10000)
  },

  props: {
    text: String,
    link: String
  }
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
</style>
