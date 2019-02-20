<template>
	<div class="voting white">
		<h2>Voting</h2>
		<div class="error" v-for="error in errors" :key="error.type">
			{{ error.description }} ({{ error.type }})
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
import Vue from 'vue';

export default {
	name: 'Voting',
	data() {
		return {
			candidates: [],
			selected: '',
			errors: {},
			submit: {
				submitting: false,
				log: []
			}
		}
	},
	created() {
		this.fetchCandidates();

		this.checkErrors();
		setInterval(function() {
			this.checkErrors();
		}.bind(this), 2000);
	},
	methods: {
		checkErrors() {
			if(!this.$store.state.web3.instance) {
				this.errors.noWeb3 = {
					type: 'no_web3_instance',
					description: 'No web3 instance'
				};
			} else {
				Vue.delete(this.errors, 'noWeb3');
			}

			if(!this.$store.state.web3.smartcontract) {
				this.errors.noSC = {
					type: 'no_smartcontract',
					description: 'No smartcontract set'
				};
			} else {
				Vue.delete(this.errors, 'noSC');
			}
		},
		fetchCandidates() {
			this.candidates.push('example');
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.voting {

}

.error {
	background-color: rgba(255,0,0, 0.7);
	width: 90%;
	border-radius: 5px;
	margin: 10px auto;
	height: 30px;
	display: flex;
	flex-direction: column;
	justify-content: center;
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
