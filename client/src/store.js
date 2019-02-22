import Vue from 'vue';
import Vuex from 'vuex';

Vue.use(Vuex);

const dev = process.env.NODE_ENV === 'development';

export default new Vuex.Store({
	state: {
		dev: dev,
		web3: {
			$instance: null, //$ prefix is to disable Vue from attaching observers to web3, which results in an error
			status: 'Retreiving status...',
			$smartcontract: null
		},
		notifications: []
	},
	mutations: {
		SET_WEB3_INSTANCE(state, instance) {
			console.log('Web3 instance set');
			state.web3.instance = instance;
		},
		SET_WEB3_DEFAULT_ACCOUNT(state, account) {
			console.log('Web3 default account set');
			state.web3.instance.eth.defaultAccount = account;
		},
		SET_NETWORK_STATUS(state, status) {
			state.web3.status = status;
		},
		SET_SMARTCONTRACT_INSTANCE(state, instance) {
			console.log('SmartContract instance set')
			state.web3.smartcontract = instance;
		},
		ADD_NOTIFICATION(state, notification) {
			state.notifications.push(notification);
		}
	},
	getters: {
		getCurrentProviderURL: (state) => {
			let web3 = state.web3.instance;

			if (web3.currentProvider.isMetaMask)
				return 'metamask';

			if (web3.currentProvider.isTrust)
				return 'trust';

			if (web3.currentProvider.isToshi)
				return 'toshi';

			if (web3.currentProvider.constructor.name === 'EthereumProvider')
				return 'mist';

			if (web3.currentProvider.constructor.name === 'Web3FrameProvider')
				return 'parity';

			if (web3.currentProvider.connection && web3.currentProvider.connection.url)
				return web3.currentProvider.connection.url;

			if (!web3.currentProvider.connected && web3.currentProvider.host)
				return web3.currentProvider.host;

			return 'unknown';
		}
	}
});
