<template>
	<router-view />
</template>

<script>
export default {
	name: 'Vote',
	mounted() {
		this.init();
	},
	beforeRouteLeave(to, from, next) {
		this.reset();
		next();
	},
	methods: {
		init() {
			// Fetch info about vote
			this.$http.get(`/getElection/${this.$route.params.id}`).then(async (result) => {
				// Error handling
				if (!result.data.nodeAddr || !result.data.contracts) {
					this.error = true;
					return;
				}

				// Do things in store and wait for them to complete
				this.$store.commit('SET_ELECTION_NAME', result.data.name);
				await this.$store.dispatch('CREATE_WEB3', result.data.nodeAddr);
				await this.$store.dispatch('CREATE_SMARTCONTRACTS', result.data.contracts);
				await this.$store.dispatch('FETCH_CANDIDATES');
				await this.$store.dispatch('FETCH_PUBLICKEY');
			}).catch((err) => {
				console.error(err);
				this.$store.commit('ADD_NOTIFICATION', { message: 'Error connecting to server', type: 'warn' });
			});
		},
		reset() {
			this.$store.dispatch('RESET_VOTE');
		}
	},
	watch: {
		'$route' () {
			// This runs when we switch between different votes
			this.reset();
			this.init();
		}
	}
}
</script>
