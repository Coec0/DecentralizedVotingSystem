import Vue from "vue";

const initialState = {
	$web3: null,
	$smartcontract: null,
	candidates: []
};

export const state = { ...initialState };

export const actions = {
	FETCH_CANDIDATES({ context, state }) {
		
	},
	RESET_CANDIDATES(context) {
		context.commit('SET_CANDIDATES', []);
	}
};

export const mutations = {
	SET_WEB3_INSTANCE(state, instance) {
		console.log('Web3 instance set');
		state.web3 = instance;
	},
	SET_WEB3_DEFAULT_ACCOUNT(state, account) {
		console.log('Web3 default account set');
		state.web3.eth.defaultAccount = account;
	},
	SET_NETWORK_STATUS(state, status) {
		state.web3.status = status;
	},
	SET_SMARTCONTRACT_INSTANCE(state, instance) {
		console.log('SmartContract instance set');
		state.smartcontract = instance;
	},
	SET_CANDIDATES(state, candidates) {
		console.log('Candidates set');
		state.candidates = candidates;
	}
};

const getters = {
	getCurrentProvider: (state) => {
		let web3 = state.web3;

		if (!web3) 
			return 'No Web3 instance';

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
};

export default {
	state,
	actions,
	mutations,
	getters
};