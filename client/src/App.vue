<template>
  <div id="app">
  	<NotificationHeader v-bind:items="notifications"></NotificationHeader>
  	<Navigation></Navigation>
    <router-view />
  </div>
</template>

<script>
import NotificationHeader from "@/components/NotificationHeader.vue";
import Navigation from "@/components/Navigation/Navigation.vue";
const Web3 = require('web3');

export default {
  name: "App",
  components: {
    Navigation,
    NotificationHeader
  },
  data () {
  	 return {
  	 	notifications: [{ message: 'Test notification!'}]
  	 } 
  },
  created () {
  	if(Web3.givenProvider) {
  		this.notifications.push({ message: 'MetaMask not detected!'});
  		return;
  	} 
  	
  	this.$store.commit('setWeb3Provider', Web3.givenProvider);
  }
}
</script>

<style>
#app {
  font-family: "Avenir", Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
}

body {
	margin: 0px;
}

a {
	color: inherit;
}
</style>
