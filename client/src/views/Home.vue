<template>
	<div class="home grey">
		 <section>
		 	<div class="content">
		 		<div id="title">
			 		Decentralized Voting System
			 	</div>
		 	</div>
		 </section>
		 <section>
		 	<div class="content">
		 		<div id="title">
			 		Step by step guide
			 	</div>
		 	</div>
		 </section>
	</div>
</template>

<script>
export default {
	name: 'home',
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
.content {
	margin-left: 10%;
	margin-right: 10%;
	padding-top: 10px;
	padding-bottom: 10px;
}

section {
	width: 100%;
	height: 200px;
}

#title {
	font-size: 32px;
	text-align: center;
	height: 15em;
}
</style>
