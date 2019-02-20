<template>
	<div class="voting white">
		<h2>Voting</h2>
		<div class="warning" v-for="warning in warnings" :key="warning.type">
			{{ warning.description }} ({{ warning.type }})
		</div>
		<div class="private-key-container">
			<h4>Private key</h4>
			<textarea rows="4"></textarea>
		</div>
		<div class="personal-number-container">
			<h4>Personal number</h4>
			<input type="text">
		</div>
		<div class="candidates-container">
			<h4>Candidates</h4>
			<select v-model="selected">
				<option disabled value="">Please select one</option>
				<option v-for="candidate in candidates" :key="candidate">{{ candidate }}</option>
			</select>
		</div>
		<div class="submit-container">
			<button class="button">Submit</button>
			<div v-if="submit.submitting" class="progress">
				<div class="loader"></div>
				<div class="log">
					<span v-for="item in submit.log">{{ item }}<br></span>
				</div>
			</div>
		</div>
	</div>
</template>

<script>
export default {
	name: 'Voting',
	data() {
		return {
			candidates: [],
			selected: '',
			warnings: {},
			submit: {
				submitting: false,
				log: []
			}
		}
	},
	created() {
		this.checkWarnings();
		this.fetchCandidates();
	},
	methods: {
		checkWarnings() {
			if(!this.$store.state.web3.instance.eth.defaultAccount) {
				this.warnings.noAccount = {
					type: 'no_default_account',
					description: 'No default account set'
				};
			}

			if(!this.$store.state.web3.smartcontract.instance) {
				this.warnings.noSCInstance = {
					type: 'no_smartcontract',
					description: 'No smartcontract set'
				};
			}

			if(!this.$store.state.web3.smartcontract.abi) {
				this.warnings.noABI = {
					type: 'no_abi',
					description: 'No ABI set'
				};
			}
		},
		fetchCandidates() {
			this.candidates.push('asd');
		}
	},
	computed: {
		smartcontract() {
			return this.$store.state.web3.smartcontract.instance;
		},
		abi() {
			return this.$store.state.web3.smartcontract.abi;
		}
	},
	watch: {
		smartcontract(newsc, oldsc) {
			this.checkWarnings();
		},
		abi(newabi, oldabi){
			this.checkWarnings();
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.voting {

}

.warning {
	background-color: rgba(255,0,0, 0.5);
	width: 90%;
	border-radius: 5px;
	margin: 10px auto;
}

.private-key-container textarea {
	width: 70%;
	resize: none;
}

.candidates-container {
	
}

.submit-container {

}

.submit-container button {
	margin: 25px 0px;
	background-color: #555555;
	border: none;
	width: 300px;
	color: white;
	padding: 15px 32px;
	text-align: center;
	text-decoration: none;
	display: inline-block;
	font-size: 16px;
	-webkit-transition-duration: 0.4s; /* Safari */
	transition-duration: 0.4s;
}

.submit-container button:hover {
	background-color: #7b7b7b;
}
</style>
