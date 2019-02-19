<template>
  <div class="databox">
    <div class="title">{{ title }}</div>
    <div class="value">{{ value }}</div>
  </div>
</template>

<script>
export default {
  name: "DataBox",
  props: {
    title: String,
    value: String
  }
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.databox {
  float: left;
  border-radius: 25px;
  background-color: #42b983;
  min-width: 22em;
  margin: 10px;
  height: 15em;
}

.value {
  font-size: 32px;
  color: white;

  text-transform: uppercase;
}
</style>
