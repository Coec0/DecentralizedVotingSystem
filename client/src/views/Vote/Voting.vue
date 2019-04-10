<template>
	<div class="container flex-grow-1 vote">
		<transition name="fade" appear>
			<h1 class="text-center">{{ name }}</h1>
		</transition>

		<transition name="fade" mode="out-in" appear>
			<private-key v-if="state.privatekey" :submit="pkSubmit"></private-key>
			<password v-if="state.password" :submit="pwSubmit"></password>
			<selection  v-if="state.selection" :submit="selectSubmit"></selection>

			<!-- Spinner -->
			<spinner v-if="state.showSpinner && state.spinnerText" v-bind:text="state.spinnerText"></spinner>
		</transition>
	</div>
</template>

<script>
// @ is an alias to /src
import PrivateKey from '@/components/Vote/PrivateKey.vue';
import Password from '@/components/Vote/Password.vue';
import Selection from '@/components/Vote/Selection.vue';
import Spinner from '@/components/Spinner.vue';
import utils from '@/utils/utils.js';

// Keep a constant for initialstate so it's easier to go back when we reset
const initialState = {
	showSpinner: false,
	spinnerText: null,
	privatekey: true,
	password: false,
	selection: false
};

export default {
	name: 'Voting',
	components: {
		'spinner': Spinner,
		'private-key': PrivateKey,
		'password': Password,
		'selection': Selection
	},
	data() {
		return {
			error: false,
			state: { ...initialState }
		};
	},
	computed: {
		name() {
			return this.$store.state.electionName
		}
	},
	methods: {
		async pkSubmit() {
			// Set the state to loading
			this.state.privatekey = false;
			this.state.showSpinner = true;
			this.state.spinnerText = 'Retreiving wallet';

			// Check if valid key
			const validKey = await utils.isValidPK(this.$store.state.web3, this.$store.state.privatekey);

			// Sleep so user can follow along
			await utils.sleep(2000);

			if (validKey) {
				// If yes, go to selection phase

				// Add sleep so that it seemes we are working hard to solve problem
				this.state.spinnerText = 'Wallet found!';
				console.log('Valid key')
				await utils.sleep(1000);

				// Set next state
				this.state.showSpinner = false;
				this.state.privatekey = false;
				this.state.selection = true;
			} else {
				// If no, go to password phase

				// Add sleep so that it seemes we are working hard to solve problem
				this.state.spinnerText = 'Wallet was not found';
				console.log('Invalid key')
				await utils.sleep(2000);

				// Set next state
				this.state.showSpinner = false;
				this.state.privatekey = false;
				this.state.password = true;
			}
		},
		async pwSubmit() {
			const pk = utils.XOR(this.$store.state.privatekey, this.$store.state.password);
			this.$store.commit('SET_PASSWORD', pk);

			this.state.showSpinner = true;
			this.state.spinnerText = 'Retreiving wallet';
			const validKey = await utils.isValidPK(this.$store.state.web3, this.$store.state.privatekey);

			if (validKey) {
				// If yes, go to selection phase
				console.log('Valid key')
				this.state.spinnerText = 'Wallet found!';
				await utils.sleep(2000);
				this.state.showSpinner = false;
				this.state.password = false;
				this.state.selection = true;
			} else {
				// If no, go back to privatekey phas
				console.log('Invalid key')
				this.state.spinnerText = 'Wallet was not found';
				await utils.sleep(2000);
				this.state.showSpinner = false;
				this.state.password = false;
				this.state.privatekey = true;
			}
		},
		async selectSubmit(selection) {
			// Error checking
			if (!selection) return console.error('selection is not valid');

			this.state.selection = false;
			this.state.spinnerText = 'Placing vote';
			this.state.showSpinner = true;
			await utils.sleep(2000);

			try {
				await this.$store.dispatch('SUBMIT_VOTE', selection);
				this.state.spinnerText = 'Vote placed!';
				await utils.sleep(2000);
				this.state.showSpinner = false;
			} catch (error) {
				console.error(error);
				this.state.spinnerText = 'Error occurred, see console';
				await utils.sleep(2000);
				this.state = { ...initialState }
			}
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
	font-size: 5rem;
	margin-top: 1em;
	margin-bottom: 1em;
}

.container {
	margin-bottom: 20px;
}

.fade-enter-active, .fade-leave-active {
	transition: opacity .5s;
}

.fade-enter, .fade-leave-to {
	opacity: 0;
}
</style>
