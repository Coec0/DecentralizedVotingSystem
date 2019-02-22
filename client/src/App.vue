<template>
	<div id="app">
		<NotificationHeader v-if="this.$store.state.notifications.length"></NotificationHeader>
		<Navigation v-bind:items="votes"></Navigation>
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
			votes: null
		}
	},
	mounted() {
		this.$http.get('/getElections').then(result => {
			this.votes = result.data;
		});

		if (Web3.givenProvider) {
			this.$store.commit('ADD_NOTIFICATION', { message: 'MetaMask detected! (Ignored)', type: 'notify' });
		}
	}
};
</script>

<style>
#app {
	font-family: 'Avenir', Helvetica, Arial, sans-serif;
	-webkit-font-smoothing: antialiased;
	-moz-osx-font-smoothing: grayscale;
	text-align: center;
	background-color: #fafafa;
}

.white {
	background-color: #fff;
}

.gray {
	background-color: #fafafa;
}

body {
	margin: 0px;
}

a {
	color: inherit;
}

</style>
