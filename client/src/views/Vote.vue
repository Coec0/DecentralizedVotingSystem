<template>
	<div class="vote gray">
		<NetworkType></NetworkType>
		<div v-if="$store.state.dev" class="dev-console">
			<div class="dev">
				<span>Dev console</span>
				<br>
				<span>Node (websocket)</span>
				<input v-model="node">
				<span>Smartcontract</span>
				<input v-model="smartcontract">
				<span>ABI</span>
				<input v-model="ABI">
				<br>
				<button v-on:click="setDev">Set</button>
			</div>
			<div class="log">
				<span v-for="item in dev.log">{{ item }}<br></span>
			</div>
		</div>
		<h1>{{ name }}</h1>
		<div class="content">
			<div class="container">
				<Voting v-bind:node="node" v-bind:blockchain="smartcontract"></Voting>
			</div>
			<div class="container">
				<Results></Results>
			</div>
			<div class="container">
				<Info v-bind:id="id" v-bind:name="name" v-bind:bc="node" v-bind:sc="smartcontract" v-bind:abi="ABI"></Info>
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
const Web3 = require('web3');

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
			node: null,
			smartcontract: null,
			ABI: null,
			dev: {
				log: []
			}
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
				this.node = result.data.nodeAddr;
				this.smartcontract = result.data.bcAddr;
				this.ABI = result.data.abi;
			});
		},
		setDev() {
			try {
				let instance = new Web3(this.node);
				this.$store.commit('SET_WEB3_INSTANCE', new Web3(this.node));
			} catch (err) {
				this.dev.log.unshift(err.message);
				this.$store.commit('SET_WEB3_INSTANCE', null);
			}
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

	width: 250px;
	height: 400px;
	background-color: white;
	border: 1px solid black;

	overflow: hidden;
}

.dev-console .dev {
	height: 60%;
	display: flex;
	flex-direction: column;
	flex-grow: 1;
	justify-content: center;
	align-items: center;
}

.dev-console .log {
	height: 40%;
	margin: 5px;
	font-size: 10px;
}

.dev input {
	width: 90%;
}

.dev button {
	background-color: #555555;
	border: none;
	color: white;
	padding: 15px 32px;
	text-align: center;
	text-decoration: none;
	display: inline-block;
	font-size: 16px;
}


</style>
