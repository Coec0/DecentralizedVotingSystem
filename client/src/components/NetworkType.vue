<template>
	<div class="network-type">
		{{ status }}
	</div>
</template>

<script>
export default {
	name: 'NetworkType',
	created() {
		this.updateStatus();

		setInterval(function() {
			this.updateStatus();
		}.bind(this), 2000);
	},
	methods: {
		updateStatus() {
			if (!this.$store.state.network.web3) {
				this.$store.commit('setNetworkStatus', 'Not connected (Web3 not initialized)');
				return;
			}

			if (!this.$store.state.network.web3.currentProvider) {
				this.$store.commit('setNetworkStatus', 'Not connected (No provider)');
				return;
			}

			this.$store.state.network.web3.eth.net.getNetworkType().then(t => {
				this.$store.commit('setNetworkStatus', `Connected to ${t} network (${this.$store.state.network.web3.currentProvider.host})`);
			}).catch((error) => {
				this.$store.commit('setNetworkStatus', `Could not connect to network (${this.$store.state.network.web3.currentProvider.host})`);
			});
		},
	},
	computed: {
		status() {
			return this.$store.state.network.status;
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.network-type {
	position: fixed; /* Sit on top of the page content */
	display: block;
	right: 0;
	bottom: 0;
	z-index: 2; /* Specify a stack order in case you're using a different order for other elements */
	color: black;
}
</style>
