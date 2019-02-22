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
			if (!this.$store.state.web3.instance) {
				this.$store.commit('SET_NETWORK_STATUS', 'Not connected (Web3 not initialized)');
				return;
			}

			if (!this.$store.state.web3.instance.currentProvider) {
				this.$store.commit('SET_NETWORK_STATUS', 'Not connected (No provider)');
				return;
			}

			if(this.$store.state.web3.instance.currentProvider.connected) {
				this.$store.state.web3.instance.eth.net.getNetworkType().then(t => {
					this.$store.commit('SET_NETWORK_STATUS', `Connected to ${t} network (${this.$store.getters.getCurrentProviderURL})`);
				}).catch((error) => {
					this.$store.commit('SET_NETWORK_STATUS', `Error fetching network type (${this.$store.getters.getCurrentProviderURL})`);
				});
			} else {
				this.$store.commit('SET_NETWORK_STATUS', `Could not connect (${this.$store.getters.getCurrentProviderURL})`);
			}
		}
	},
	computed: {
		status() {
			return this.$store.state.web3.status;
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
