<template>
  <div class="home">
  	<Navigation></Navigation>
  	<div class="content">
  		<DataBox v-for="box in boxes" v-bind:title="box.title" v-bind:value="box.value" />
  	</div>
  </div>
</template>

<script>
// @ is an alias to /src
import DataBox from "@/components/DataBox.vue";
import Navigation from "@/components/Navigation/Navigation.vue";
const Web3 = require('web3');
const web3 = new Web3(Web3.givenProvider);

export default {
  name: "home",
  components: {
    DataBox,
    Navigation
  },
  data () {
  	return {
  		boxes: [
  		{ title: "Network Type", value: null },
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
  		web3.eth.net.getNetworkType().then(type => {
  			this.boxes[0].value = type;
  		});
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
}
</style>
