<template>
	<div class="container vote">
		<h1 class="text-center">{{ name }}</h1>

	</div>
</template>

<script>
// @ is an alias to /src
import utils from '../utils/utils.js';
const Web3 = require('web3');

export default {
	name: 'Vote',
	data() {
		return {
			name: null
		};
	},
	mounted() {
		this.init();
	},
	methods: {
		init() {
			this.$http.get(`/getElection/${this.$route.params.id}`).then(async (result) => {
				this.name = result.data.name;

				await this.$store.dispatch('CREATE_WEB3', result.data.nodeAddr);
				await this.$store.dispatch('CREATE_SMARTCONTRACT', { abi: result.data.abi, scAddr: result.data.bcAddr });
				await this.$store.dispatch('FETCH_CANDIDATES');

			}).catch((err) => {
				console.error(err);
				this.$store.commit('ADD_NOTIFICATION', { message: err, type: 'warn' })
			});
		},
		reset() {
			this.name = null;
			this.$store.dispatch('RESET_VOTE');
		}
	},
	watch: {
		'$route' () {
			this.reset();
			this.init();
		}
	}
};
</script>

<style scoped>
.vote {
	width: 100%;
	height: 100%;
	color: black;
}

.title {
	height: 200px;
}

h1 {
	font-size: 5rem;
	margin-top: 2em;
	margin-bottom: 2em;
}

.container {
	margin-bottom: 20px;
}

.content {
	padding: 40px;
	margin: auto;
	background-color: rgba(44,62,80, 0.1);
}
</style>
