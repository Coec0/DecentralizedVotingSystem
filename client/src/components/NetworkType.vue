<template>
	<div id="network-type">
		Connected ({{ type }})
	</div>
</template>

<script>
export default {
	name: 'NetworkType',
	data() {
		return {
			type: null
		};
	},
	created() {
		this.updateStatus();

		setInterval(function () {
			this.updateStatus();
		}.bind(this), 2000);
	},
	methods: {
		updateStatus() {
			if (!this.$store.state.web3) {
				this.type = 'Web3 not initialized';
				return;
			}

			if (!this.$store.state.web3.currentProvider) {
				this.type = 'No provider';
				return;
			}

			this.$store.state.web3.eth.net.getNetworkType().then(t => {
				this.type = t;
			});
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
#network-type {
	position: fixed; /* Sit on top of the page content */
	display: block; /* Hidden by default */
	right: 0;
	bottom: 0;
	z-index: 2; /* Specify a stack order in case you're using a different order for other elements */
	cursor: default; /* Add a pointer on hover */
	color: black;
}
</style>
