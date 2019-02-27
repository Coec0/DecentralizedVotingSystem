<template>
	<div class="container vote gray">
		<NetworkType></NetworkType>
		<h1 class="text-center">{{ name }}</h1>
		<div class="content row">
			<div class="container">
				<VotePanel v-bind:candidates="candidates"></VotePanel>
			</div>
			<div class="container">
				<ResultPanel v-bind:results="results"></ResultPanel>
			</div>
			<div class="container">
				<InfoPanel v-bind:id="id" v-bind:name="name" v-bind:bc="node" v-bind:sc="smartcontract" v-bind:abi="ABI"></InfoPanel>
			</div>
		</div>
	</div>
</template>

<script>
// @ is an alias to /src
import NetworkType from '@/components/Vote/NetworkType.vue';
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
			results: [],
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
			}).catch(err => this.$store.commit('ADD_NOTIFICATION', { message: err.message, type: 'warn' }));	
		},
		reset() {
			this.id = null;
			this.name = null;
			this.node = null;
			this.smartcontract = null;
			this.ABI = null;
			this.candidates = [];
			this.results = [];
		},
		fetchData() {
			return new Promise((resolve, reject) => {
				this.$http.get(`/getElection/${this.$route.params.id}`).then(result => {
					this.id = result.data.id;
					this.name = result.data.name;
					this.node = 'ws://localhost:7545';
					this.smartcontract = '0x74786d96fad14ca646aacd52145bb3b8f703f051';
					this.ABI = '[ { "constant": false, "inputs": [ { "name": "id", "type": "uint256" } ], "name": "vote", "outputs": [], "payable": false, "stateMutability": "nonpayable", "type": "function" }, { "constant": false, "inputs": [ { "name": "candidate", "type": "bytes32" } ], "name": "addCandidate", "outputs": [], "payable": false, "stateMutability": "nonpayable", "type": "function" }, { "constant": true, "inputs": [ { "name": "", "type": "uint256" } ], "name": "idToIndexMap", "outputs": [ { "name": "", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [ { "name": "id", "type": "uint256" } ], "name": "debugGetCandidateStringNameID", "outputs": [ { "name": "", "type": "string" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [ { "name": "", "type": "address" } ], "name": "votedOn", "outputs": [ { "name": "", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": false, "inputs": [], "name": "debugAddTestWhitelistVoters", "outputs": [], "payable": false, "stateMutability": "nonpayable", "type": "function" }, { "constant": true, "inputs": [], "name": "candidateCount", "outputs": [ { "name": "", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [], "name": "getCandidateInLead", "outputs": [ { "name": "id", "type": "uint256" }, { "name": "name", "type": "bytes32" }, { "name": "votes", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [], "name": "blocksLeft", "outputs": [ { "name": "", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [], "name": "isVotingOpen", "outputs": [ { "name": "", "type": "bool" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [ { "name": "", "type": "uint256" } ], "name": "allCandidates", "outputs": [ { "name": "id", "type": "uint256" }, { "name": "name", "type": "bytes32" }, { "name": "votecount", "type": "uint256" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "constant": true, "inputs": [ { "name": "index", "type": "uint256" } ], "name": "debugGetCandidateStringNameIdx", "outputs": [ { "name": "", "type": "string" } ], "payable": false, "stateMutability": "view", "type": "function" }, { "inputs": [ { "name": "candidates", "type": "bytes32[]" }, { "name": "blockamount", "type": "uint256" }, { "name": "store", "type": "address" } ], "payable": false, "stateMutability": "nonpayable", "type": "constructor" } ]';
					resolve();
				}).catch((err) => reject(err));
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
						let name = this.$store.state.web3.instance.utils.hexToAscii(utils.removeTrailingZeroes(candidate.name));
						this.candidates.push({
							id: candidate.id,
							name: name,
							votecount: parseInt(candidate.votecount)
						});

						this.results.push({
							name: name,
							y: parseInt(candidate.votecount)
						})
					});
				}
			}).catch(console.error);
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

.container {
	margin-bottom: 20px;
}

.content {
	padding: 40px;
	margin: auto;
	background-color: rgba(44,62,80, 0.1);
}
</style>
