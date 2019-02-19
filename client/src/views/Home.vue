<template>
	<div class="home">
		<NetworkType></NetworkType>
		<div class="content">
			 <section id="title">
			 	<div>
			 		Decentralized Voting System
			 	</div>
			 </section>
		</div>
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
		return {
			boxes: [
				{ id: 1, title: '', value: null },
				{ id: 2, title: '', value: null },
				{ id: 3, title: '', value: null },
				{ id: 4, title: '', value: null },
				{ id: 5, title: '', value: null },
				{ id: 6, title: '', value: null }
			]
		};
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
	background: white;
	display: flex;
	justify-content: space-between;
	flex-wrap: wrap;
	margin-left: 10%;
	margin-right: 10%;
	margin-top: 20px;
}

#title {
	font-size: 32px;
	text-align: center;
}
</style>
