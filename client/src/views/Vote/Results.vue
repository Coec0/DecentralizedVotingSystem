<template>
	<div class="home">
	 	<div class="container">{{ results }}</div>
	</div>
</template>

<script>
export default {
	name: 'Results',
	data() {
		return {
			results: 'RESULTS NOT LOADED'
		};
	},
	created() {
		this.fetchResults();
	},
	methods: {
		fetchResults() {
			this.$http.get(`/getElectionResults/${this.$route.params.id}`).then((results) => {
				this.results = results.data;
			}).catch((error) => {
				console.error(error);
				this.results = error.message;
			});
		}
	}
};
</script>

<style scoped>
.home {
	width: 100%;
}

.container {
	height: 100px;
}

#title {
	font-size: 32px;
	text-align: center;
	height: 15em;
}
</style>
