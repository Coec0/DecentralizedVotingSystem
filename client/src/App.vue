<template>
	<div id="app">
		<NotificationHeader v-if="notifications.length" v-bind:items="notifications"></NotificationHeader>
		<Navigation></Navigation>
		<router-view />
	</div>
</template>

<script>
import NotificationHeader from '@/components/NotificationHeader.vue';
import Navigation from '@/components/Navigation/Navigation.vue';
const Web3 = require('web3');

export default {
	name: 'App',
	components: {
		Navigation,
		NotificationHeader
	},
	data() {
		return {
			notifications: []
		};
	},
	created() {
		if (!Web3.givenProvider) {
			this.notifications.push({ message: 'MetaMask not detected!' });
			return;
		}

		this.$store.commit('setWeb3Provider', Web3.givenProvider);
	}
};
</script>

<style>
#app {
	font-family: 'Avenir', Helvetica, Arial, sans-serif;
	-webkit-font-smoothing: antialiased;
	-moz-osx-font-smoothing: grayscale;
	text-align: center;
	color: #2c3e50;
}

.primary-color {
	background-color: #2c3e50;
}

.secondary-color {
	background-color: #42b983;
}

div {
	color: white;
}

body {
	margin: 0px;
}

a {
	color: inherit;
}
</style>
