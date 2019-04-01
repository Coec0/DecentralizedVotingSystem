<template>
	<div class="container vote ">
		<div class="error" v-if="error">
			<h2>Vote hasn't opened yet!</h2>
		</div>

		<div v-else>
			<transition name="fade" appear>
				<h1 v-if="loaded" class="text-center">{{ name }}</h1>
			</transition>

			<transition name="fade" mode="out-in" appear>
				<private-key v-if="loaded && state.privatekey" :submit="pkSubmit"></private-key>
				<password v-if="loaded && state.password" :submit="pwSubmit"></password>
				<selection  v-if="loaded && state.selection" :submit="selectSubmit"></selection>

				<!-- Spinner -->
				<spinner v-if="state.showSpinner && state.spinnerText" v-bind:text="state.spinnerText"></spinner>
			</transition>
		</div>
	</div>
</template>

<script>
// @ is an alias to /src
import PrivateKey from '@/components/Vote/PrivateKey.vue';
import Password from '@/components/Vote/Password.vue';
import Selection from '@/components/Vote/Selection.vue';
import Spinner from '@/components/Spinner.vue';
import utils from '../utils/utils.js';

// Keep a constant for initialstate so it's easier to go back when we reset
const initialState = {
	showSpinner: false,
	spinnerText: null,
	privatekey: true,
	password: false,
	selection: false
};

export default {
	name: 'Vote',
	components: {
		'spinner': Spinner,
		'private-key': PrivateKey,
		'password': Password,
		'selection': Selection
	},
	data() {
		return {
			name: null,
			loaded: false,
			error: false,
			state: { ...initialState }
		};
	},
	mounted() {
		this.init();
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
			const pk = utils.decryptPK(this.$store.state.privatekey, this.$store.state.password);
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
		},
		init() {
			// Fetch info about vote
			this.$http.get(`/getElection/${this.$route.params.id}`).then(async (result) => {
				// Error handling
				if (!result.data.nodeAddr || !result.data.abi || !result.data.bcAddr) {
					this.error = true;
					return;
				}

				// Store name so we can show it
				this.name = result.data.name;

				// Do things in store and wait for them to complete
				await this.$store.dispatch('CREATE_WEB3', result.data.nodeAddr);
				await this.$store.dispatch('CREATE_SMARTCONTRACT', { abi: result.data.abi, scAddr: result.data.bcAddr });
				await this.$store.dispatch('FETCH_CANDIDATES');

				// Show components
				this.loaded = true;
			}).catch(console.error);
		},
		reset() {
			this.name = null;
			this.loaded = false;
			this.error = false;
			this.state = { ...initialState };
			this.$store.dispatch('RESET_VOTE');
		}
	},
	watch: {
		'$route' () {
			// This runs when we switch between different votes
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
	font-size: 5rem;
	margin-top: 1em;
	margin-bottom: 1em;
}

.container {
	margin-bottom: 20px;
}

.content {
	padding: 40px;
	margin: auto;
	background-color: rgba(44,62,80, 0.1);
}

.fade-enter-active, .fade-leave-active {
	transition: opacity .5s;
}

.fade-enter, .fade-leave-to {
	opacity: 0;
}
</style>
