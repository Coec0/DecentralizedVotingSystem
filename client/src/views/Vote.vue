<template>
	<div class="container vote">
		<transition name="fade" appear>
			<h1 v-if="loaded" class="text-center">{{ name }}</h1>
		</transition>

		<transition name="fade" mode="out-in" appear>
			<private-key v-if="loaded && state.privatekey" :submit="pkSubmit"></private-key>
			<password v-if="loaded && state.password" :submit="pwSubmit"></password>
			<selection  v-if="loaded && state.selection" :submit="selectSubmit"></selection>
		</transition>		
	</div>
</template>

<script>
// @ is an alias to /src
import PrivateKey from '@/components/Vote/PrivateKey.vue';
import Password from '@/components/Vote/Password.vue';
import Selection from '@/components/Vote/Selection.vue';
import utils from '../utils/utils.js';

// Keep a constant for initialstate so it's easier to go back when we reset
const initialState = {
	privatekey: true,
	password: false,
	selection: false
};

export default {
	name: 'Vote',
	components: {
		'private-key': PrivateKey,
		'password': Password,
		'selection': Selection
	},
	data() {
		return {
			name: null,
			loaded: false,
			state: { ...initialState }
		};
	},
	mounted() {
		this.init();
	},
	methods: {
		async pkSubmit() {
			// Check if valid key
			const validKey = await utils.isValidPK(this.$store.state.web3, this.$store.state.privatekey);

			// Add sleep so that it seemes we are working hard to solve problem
			await utils.sleep(1000);

			if (validKey) {
				// If yes, go to selection phase
				console.log('Valid key')
				this.state.privatekey = false;
				this.state.selection = true;
			} else {
				// If no, go to password phase
				console.log('Invalid key')
				this.state.privatekey = false;
				this.state.password = true;
			}
		},
		async pwSubmit() {
			const pk = utils.decryptPK(this.$store.state.privatekey, this.$store.state.password);
			this.$store.commit('SET_PASSWORD', pk);

			const validKey = await utils.isValidPK(this.$store.state.web3, this.$store.state.privatekey);

			if (validKey) {
				// If yes, go to selection phase
				console.log('Valid key')
				this.state.password = false;
				this.state.selection = true;
			} else {
				// If no, go back to privatekey phas
				console.log('Invalid key')
				this.state.password = false;
				this.state.privatekey = true;
			}
		},
		async selectSubmit() {
			console.log('Vote::selectSubmit')
			// Todo
		},
		init() {
			// Fetch info about vote
			this.$http.get(`/getElection/${this.$route.params.id}`).then(async (result) => {
				// Store name so we can show it
				this.name = result.data.name;

				// Do things in store and wait for them to complete
				await this.$store.dispatch('CREATE_WEB3', result.data.nodeAddr);
				await this.$store.dispatch('CREATE_SMARTCONTRACT', { abi: result.data.abi, scAddr: result.data.bcAddr });
				await this.$store.dispatch('FETCH_CANDIDATES');

				// Show components
				this.loaded = true;
			}).catch((err) => {
				console.error(err);
			});
		},
		reset() {
			this.name = null;
			this.loaded = false;
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

.fade-enter-active, .fade-leave-active {
	transition: opacity .5s;
}

.fade-enter, .fade-leave-to {
	opacity: 0;
}
</style>
