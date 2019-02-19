import Vue from 'vue';
import Vuex from 'vuex';
const Web3 = require('web3');
const axios = require('axios');

Vue.use(Vuex);

console.log(process.env)

const axiosInstance = axios.create({
	baseURL: process.env.NODE_ENV === 'development' ? 'http://localhost:8080/api' : 'http://URL.com/api',
	timeout: 1000
});

export default new Vuex.Store({
	state: {
		$web3: null, //$ prefix is to disable Vue from attaching observers to web3, which yields an error
		axios: axiosInstance
	},
	mutations: {
		setWeb3Provider(state, provider) {
			state.web3 = new Web3(provider);
		}
	}
});
