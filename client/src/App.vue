<template>
	<div id="app">
		<NotificationHeader v-if="notifications.length" v-bind:items="notifications"></NotificationHeader>
		<Navigation v-bind:items="activeElections"></Navigation>
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
			notifications: [],
			activeElections: null
		};
	},
	created() {
		
	},
	mounted() {
		if (!Web3.givenProvider) {
			this.notifications.push({ message: 'MetaMask not detected!' });
			return;
		}
		
		// this.$store.state.axios.get('/hello').then(result => {
		// 	console.log(result)
		// });

		this.activeElections = [
			{ id: 1, name: 'Riksdagsval', node: 'localhost:1234', bcAdr: 0x123123123 },
			{ id: 2, name: 'Landstingsval', node: 'localhost:1234', bcAdr: 0x247821238 }
		];

		// this.$store.commit('setWeb3Provider', Web3.givenProvider);
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
