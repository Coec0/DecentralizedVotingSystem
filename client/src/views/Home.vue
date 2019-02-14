<template>
  <div class="home">
  	<NetworkType></NetworkType>
  	<div class="content">
  		<DataBox v-for="box in boxes" v-bind:title="box.title" v-bind:value="box.value" />
  	</div>
  </div>
</template>

<script>
// @ is an alias to /src
import DataBox from "@/components/DataBox.vue";
import NetworkType from "@/components/NetworkType.vue";

export default {
  name: "home",
  components: {
    DataBox,
    NetworkType
  },
  data () {
  	return {
  		boxes: [
  		{ title: "", value: null },
  		{ title: "", value: null },
  		{ title: "", value: null },
  		{ title: "", value: null },
  		{ title: "", value: null },
  		{ title: "", value: null }]
  	}
  },
  created () {
  	this.fetchData();
  },
  methods: {
  	fetchData() {
  		
  	}
  }
};
</script>

<style scoped>
.home {
	width: 100%;
}
.content {
	display: flex;
	justify-content: space-between;
	flex-wrap: wrap;
	margin-left: 10%;
	margin-right: 10%;
  margin-top: 20px;
}
</style>
