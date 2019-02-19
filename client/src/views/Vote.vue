<template>
	<div class="home">
		{{ $route.params.id }}
	</div>
</template>

<script>
export default {
	name: 'vote',
	data() {
		return {
			name: null,
			nodeAddress: null,
			blockChainAddress: null,
			ABI: null
		};
	},
	created() {
		this.fetchData();
	},
	methods: {
		fetchData() {
			this.$store.state.axios.get(`/getElectionInfo/${this.$route.params.id}`).then(result => console.log(result));
		}
	},
	watch: {
		'$route' (to, from) {
			this.fetchData();
		}
	}
};
</script>

<style scoped>
.home {
	width: 100%;
	background: white;
	color: black;
}
</style>
