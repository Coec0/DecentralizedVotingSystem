<template>
	<div class="vote gray">
		<NetworkType></NetworkType>
		<div v-if="$store.state.dev" class="dev-console">
			<div class="dev">
				<span>Dev console</span>
				<div>
					<span>Node (websocket)</span>
					<input v-model="node">
					<button v-on:click="connectToNode">Connect</button>
				</div>
				
				<div>
					<span>Smartcontract</span>
					<input v-model="smartcontract">
					<span>ABI</span>
					<input v-model="ABI">
					<br>
					<button v-on:click="setContract">Set Contract</button>
				</div>
			</div>
			<div class="log">
				<span v-for="item in dev.log">{{ item }}<br></span>
			</div>
		</div>
		<h1>{{ name }}</h1>
		<div class="content">
			<div class="container">
				<VotePanel v-bind:candidates="candidates"></VotePanel>
			</div>
			<div class="container">
				<ResultPanel></ResultPanel>
			</div>
			<div class="container">
				<InfoPanel v-bind:id="id" v-bind:name="name" v-bind:bc="node" v-bind:sc="smartcontract" v-bind:abi="ABI"></InfoPanel>
			</div>
		</div>
	</div>
</template>

<script>
// @ is an alias to /src
import NetworkType from '@/components/NetworkType.vue';
import VotePanel from '@/components/Vote/VotePanel.vue';
import ResultPanel from '@/components/Vote/ResultPanel.vue';
import InfoPanel from '@/components/Vote/InfoPanel.vue';
import utils from '../utils/utils.js';
const Web3 = require('web3');

export default {
	name: 'vote',
	components: {
		NetworkType,
		VotePanel,
		ResultPanel,
		InfoPanel
	},
	data() {
		return {
			id: null,
			name: null,
			node: null,
			smartcontract: null,
			ABI: null,
			candidates: [],
			dev: {
				log: []
			}
		};
	},
	mounted() {
		this.init();
	},
	methods: {
		init() {
			this.fetchData().then(() => {
				this.connectToNode();
				this.setContract();
				this.fetchCandidates();
			});	
		},
		reset() {
			this.id = null;
			this.name = null;
			this.node = null;
			this.smartcontract = null;
			this.ABI = null;
			this.candidates = [];
		},
		fetchData() {
			return new Promise((resolve, reject) => {
				this.$http.get(`/getElection/${this.$route.params.id}`).then(result => {
					this.id = result.data.id;
					this.name = result.data.name;
					this.node = 'ws://localhost:7545';
					this.smartcontract = '0xbf1c5765869fa0d606bf14667f65f8b61a6dddcb';
					this.ABI = '[ 	{ 		"constant": false, 		"inputs": [ 			{ 				"name": "candidate", 				"type": "bytes32" 			} 		], 		"name": "addCandidate", 		"outputs": [], 		"payable": false, 		"stateMutability": "nonpayable", 		"type": "function" 	}, 	{ 		"constant": false, 		"inputs": [], 		"name": "debugAddTestWhitelistVoters", 		"outputs": [], 		"payable": false, 		"stateMutability": "nonpayable", 		"type": "function" 	}, 	{ 		"constant": false, 		"inputs": [ 			{ 				"name": "id", 				"type": "uint256" 			} 		], 		"name": "vote", 		"outputs": [], 		"payable": false, 		"stateMutability": "nonpayable", 		"type": "function" 	}, 	{ 		"inputs": [ 			{ 				"name": "candidates", 				"type": "bytes32[]" 			}, 			{ 				"name": "blockamount", 				"type": "uint256" 			}, 			{ 				"name": "store", 				"type": "address" 			} 		], 		"payable": false, 		"stateMutability": "nonpayable", 		"type": "constructor" 	}, 	{ 		"constant": true, 		"inputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"name": "allCandidates", 		"outputs": [ 			{ 				"name": "id", 				"type": "uint256" 			}, 			{ 				"name": "name", 				"type": "bytes32" 			}, 			{ 				"name": "votecount", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [], 		"name": "blocksLeft", 		"outputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [], 		"name": "candidateCount", 		"outputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [ 			{ 				"name": "id", 				"type": "uint256" 			} 		], 		"name": "debugGetCandidateStringNameID", 		"outputs": [ 			{ 				"name": "", 				"type": "string" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [ 			{ 				"name": "index", 				"type": "uint256" 			} 		], 		"name": "debugGetCandidateStringNameIdx", 		"outputs": [ 			{ 				"name": "", 				"type": "string" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [], 		"name": "getCandidateInLead", 		"outputs": [ 			{ 				"name": "id", 				"type": "uint256" 			}, 			{ 				"name": "name", 				"type": "bytes32" 			}, 			{ 				"name": "votes", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"name": "idToIndexMap", 		"outputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [], 		"name": "isVotingOpen", 		"outputs": [ 			{ 				"name": "", 				"type": "bool" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	}, 	{ 		"constant": true, 		"inputs": [ 			{ 				"name": "", 				"type": "address" 			} 		], 		"name": "votedOn", 		"outputs": [ 			{ 				"name": "", 				"type": "uint256" 			} 		], 		"payable": false, 		"stateMutability": "view", 		"type": "function" 	} ]';
					resolve();
				}).catch(() => reject());
			});
		},
		connectToNode() {
			try {
				let web3Instance = new Web3(this.node);
				this.$store.commit('SET_WEB3_INSTANCE', web3Instance);
			} catch (err) {
				console.log(err);
				this.$store.commit('SET_WEB3_INSTANCE', null);
			}
		},
		setContract() {
			try {
				let parsedABI = JSON.parse(this.ABI);
				let contractInstance = this.$store.state.web3.instance.eth.Contract(parsedABI, this.smartcontract);
				this.$store.commit('SET_SMARTCONTRACT_INSTANCE', contractInstance);
				window.sc = contractInstance;
			} catch (err) {
				console.log(err);
				this.$store.commit('SET_SMARTCONTRACT_INSTANCE', null);
			}
		},
		fetchCandidates() {
			this.$store.state.web3.smartcontract.methods.candidateCount().call().then(count => {
				for (var i = 0; i < count; i++) {
					this.$store.state.web3.smartcontract.methods.allCandidates(i).call().then(candidate => {
						this.candidates.push({
							id: candidate.id,
							name: this.$store.state.web3.instance.utils.hexToAscii(utils.removeTrailingZeroes(candidate.name)),
							votecount: candidate.votecount
						});
					});
				}
			});
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
	top: 100px;
	z-index: 2; /* Specify a stack order in case you're using a different order for other elements */
	color: black;

	width: 250px;
	height: 450px;
	background-color: white;
	border: 1px solid black;

	overflow: hidden;
}

.dev-console .dev {
	height: 60%;
	display: inline-block;
}

.dev-console .dev div {
	margin: 25px 0px;
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
