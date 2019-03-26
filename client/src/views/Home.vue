<template>
	<div class="home white">
	 	<div class="container">
	 		<div id="title">
		 		Decentralized Voting System
		 	</div>
	 	</div>
	 	<div class="container">
	 		<div id="title">
		 		Step by step guide
		 	</div>
	 	</div>
	</div>
</template>

<script>
export default {
	name: 'Home',
	data() {
		return {};
	},
	created() {
		this.fetchData();
	},
	methods: {
		fetchData() {}
	}
};
</script>

<style scoped>
.home {
	width: 100%;
}

#title {
	font-size: 32px;
	text-align: center;
	height: 15em;
}
</style>
