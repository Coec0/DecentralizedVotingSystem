<template>
	<div class="vote gray">
		<NetworkType></NetworkType>
		<div class="title text-center">
			<h1>{{ name }}</h1>
		</div>
		<div class="content">
			{{ $route.params.id }}
			<div class="container">
				<Voting></Voting>
			</div>
			<div class="container">
				<Results></Results>
			</div>
			<div class="container">
				<Info v-bind:id="id" v-bind:name="name" v-bind:bc="nodeAddress" v-bind:sc="blockChainAddress" v-bind:abi="ABI"></Info>
			</div>
		</div>
	</div>
</template>

<script>
// @ is an alias to /src
import NetworkType from '@/components/NetworkType.vue';
import Voting from '@/components/Vote/Voting.vue';
import Results from '@/components/Vote/Results.vue';
import Info from '@/components/Vote/Info.vue';

export default {
	name: 'vote',
	components: {
		NetworkType,
		Voting,
		Results,
		Info
	},
	data() {
		return {
			id: null,
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
			this.$store.state.axios.get(`/getElection/${this.$route.params.id}`).then(result => {
				this.id = result.data.id;
				this.name = result.data.name;
				this.nodeAddress = result.data.nodeAddr;
				this.blockChainAddress = result.data.bcAddr;
				this.ABI = result.data.abi;
			});
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
.vote {
	width: 100%;
	height: 100%;
	color: black;
}

.title {
	margin-top: 100px;
}

.title h1 {
	font-size: 3rem;
}

.content {
	margin-top: 100px;
	width: 60%;
	margin: auto;
	background-color: rgba(44,62,80, 0.3);
	height: 700px;
}

.container {
	margin: 50px 50px;
}

</style>
