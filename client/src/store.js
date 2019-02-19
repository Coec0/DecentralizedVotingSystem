import Vue from 'vue';
import Vuex from 'vuex';
const Web3 = require('web3');

Vue.use(Vuex);

export default new Vuex.Store({
	state: {
		$web3: null, //$ prefix is to disable Vue from attaching observers to web3, which yields an error
		axios: null
	},
	mutations: {
		setWeb3Provider(state, provider) {
			state.web3 = new Web3(provider);
		},
		setAxios(state, axios) {
			state.axios = axios;
		}
	}
});
