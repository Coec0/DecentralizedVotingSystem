import Vue from 'vue';
import Vuex from 'vuex';
const Web3 = require('web3');
const axios = require('axios');

Vue.use(Vuex);

const dev = process.env.NODE_ENV === 'development';

const axiosInstance = axios.create({
	baseURL: 'http://localhost:8080/api',
	timeout: 5000
});

export default new Vuex.Store({
	state: {
		dev: dev,
		network: {
			$web3: null, //$ prefix is to disable Vue from attaching observers to web3, which results in an error
			status: 'Retreiving status...'
		},
		axios: axiosInstance
	},
	mutations: {
		setWeb3Provider(state, provider) {
			if(state.network.web3) {
				state.network.web3.setProvider(provider);
			} else {
				state.network.web3 = new Web3(provider);
			}
		},
		setWeb3HttpProvider(state, provider) {
			if(state.network.web3) {
				state.network.web3.setProvider(new Web3.providers.HttpProvider(provider));
			} else {
				state.network.web3 = new Web3(new Web3.providers.HttpProvider(provider));
			}
		},
		setNetworkStatus(state, status) {
			state.network.status = status;
		}
	}
});
