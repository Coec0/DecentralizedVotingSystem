<template>
	<div class="vote gray">
		<NetworkType></NetworkType>
		<div v-if="$store.state.dev" class="dev-console">
			<span>Dev console</span><br>
			<span>Node: </span><br><input v-model="nodeAddress"><br><br>
			<span>Sc: </span><br><input v-model="blockChainAddress"><br><br>
			<span>ABI: </span><br><input v-model="ABI"><br><br>
			<button v-on:click="setDev">Set</button>
		</div>
		<h1>{{ name }}</h1>
		<div class="content">
			<div class="container">
				<Voting v-bind:node="nodeAddress" v-bind:blockchain="blockChainAddress"></Voting>
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
		},
		setDev() {
			this.$store.state.network.web3.eth.getAccounts(console.log)
			//this.$store.commit('setWeb3HttpProvider', this.nodeAddress);
		}
	},
	watch: {
		'$route' () {
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
	height: 200px;
}

h1 {
	font-size: 3rem;
	margin-top: 2em;
	margin-bottom: 2em;
}

.content {
	padding: 50px;
	width: 60%;
	margin: auto;
	background-color: rgba(44,62,80, 0.3);
	height: 1000px;
}

.container {
	margin-bottom: 50px;
}

.dev-console {
	position: fixed; /* Sit on top of the page content */
	display: block;
	right: 10px;
	top: 60px;
	z-index: 2; /* Specify a stack order in case you're using a different order for other elements */
	color: black;

	width: 300px;
	height: 200px;
	background-color: white;
	border: 1px solid black;
}

.dev-console input {
	width: 90%;
}

.dev-console button {
	background-color: #555555;
	border: none;
	color: white;
	padding: 15px 32px;
	text-align: center;
	text-decoration: none;
	display: inline-block;
	font-size: 16px;}


</style>
