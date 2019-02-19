<template>
	<div class="home">
		<NetworkType></NetworkType>
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
// @ is an alias to /src
import NetworkType from '@/components/NetworkType.vue';

export default {
	name: 'home',
	components: {
		NetworkType
	},
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
	background: white;
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

section:nth-child(odd) {
	background: #42b983;
}

#title {
	font-size: 32px;
	text-align: center;
	color: black;
	height: 15em;
}
</style>
