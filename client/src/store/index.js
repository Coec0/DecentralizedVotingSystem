const Web3 = require('web3');
import Vue from 'vue';
import Vuex from 'vuex';

import state from './state';
import utils from './../utils/utils.js';

Vue.use(Vuex);

export default new Vuex.Store({
	strict: false,	// Cannot use strict-mode as it isn't compatible with Web3.Contract
	state,
	mutations: {
		ADD_NOTIFICATION(state, notification) {
			if (!state.notifications.some(n => n.message === notification.message)) {
				state.notifications.push(notification);
			}
		},
		CLEAR_NOTIFICATIONS(state) {
			state.notifications = [];
		},
		SET_WEB3(state, instance) {
			state.web3 = instance;
			console.log('Web3 instance set');
		},
		SET_WEB3_DEFAULT_ACCOUNT(state, account) {
			// console.log('Web3 default account set');
			// state.web3.eth.defaultAccount = account;
		},
		SET_SMARTCONTRACT(state, instance) {
			state.smartcontract = instance;
			console.log('SmartContract instance set');
		},
		SET_CANDIDATES(state, candidates) {
			state.candidates = candidates;
			console.log(`Candidates set (${candidates.map(c => ' ' + c.name)})`);
		},
		SET_PRIVATEKEY(state, key) {
			state.privatekey = key;
		},
		SET_PASSWORD(state, pw) {
			state.password = pw;
		}
	},
	actions: {
		CREATE_WEB3({ commit, state }, node) {
			return new Promise(async (resolve, reject) => {
				if (!node) return reject('Tried CREATE_WEB3 without node passed as parameter');

				try {
					let instance = new Web3(node);
					commit('SET_WEB3', instance);
					resolve(instance);
				} catch (err) {
					// commit('SET_WEB3', null);
					reject(err);
				}
			});
		},
		CREATE_SMARTCONTRACT({ commit, state }, options) {
			return new Promise(async (resolve, reject) => {
				// Error checking
				if (typeof options !== 'object') return console.error('Object needs to be passed as options');
				if (!options.scAddr) return console.error('No smartcontract adress specified');
				if (!options.abi) return console.error('No smartcontract ABI specified');
				if (!state.web3) return reject('Tried CREATE_SMARTCONTRACT without web3 set');

				try {
					// Parse JSON string
					let parsedABI = JSON.parse(options.abi);

					// Create smartcontract object
					let contract = new state.web3.eth.Contract(parsedABI, options.scAddr);

					// Add SC to state
					commit('SET_SMARTCONTRACT', contract);

					// Tell caller to go on
					resolve(contract);

					// This if for debugging
					window.sc = contract;
				} catch (err) {
					commit('SET_SMARTCONTRACT', null);
					reject(err);
				}
			});
		},
		FETCH_CANDIDATES({ commit, state }) {
			return new Promise(async (resolve, reject) => {
				// Error checking
				if (!state.web3) return reject('Tried FETCH_CANDIDATES without web3 set');
				if (!state.smartcontract) return reject('Tried FETCH_CANDIDATES without smartcontract set');
				const candidates = [];

				try {
					const candidateCount = await state.smartcontract.methods.candidateCount().call();

					for (var i = 0; i < candidateCount; i++) {
						const candidate = await state.smartcontract.methods.allCandidates(i).call();

						candidates.push({
							id: candidate.id,
							name: state.web3.utils.hexToAscii(utils.removeTrailingZeroes(candidate.name))
						});
					}
				} catch (error) {
					return reject(error);
				}

				// Push candidate list to state
				commit('SET_CANDIDATES', candidates);
				// Tell caller we succeeded
				resolve();
			});
		},
		RESET_VOTE({ commit, state }) {
			commit('SET_CANDIDATES', []);
			commit('SET_PRIVATEKEY', null);
			commit('SET_WEB3', null);
			commit('SET_SMARTCONTRACT', null);	
		},
		async PRINT_RESULTS({ commit, state }) {
			// Error checking
			if (!state.web3) return reject('Tried PRINT_RESULTS without web3 set');
			if (!state.smartcontract) return reject('Tried PRINT_RESULTS without smartcontract set');
			const candidates = [];
			let totalVotes = 0;

			try {
				const candidateCount = await state.smartcontract.methods.candidateCount().call();

				for (var i = 0; i < candidateCount; i++) {
					const candidate = await state.smartcontract.methods.allCandidates(i).call();
					let votecount = parseInt(candidate.votecount, 10);

					totalVotes += votecount;

					candidates.push({
						votecount,
						name: state.web3.utils.hexToAscii(utils.removeTrailingZeroes(candidate.name))
					});
				}
			} catch (err) {
				console.error(err);
			}

			console.log('Results:');
			console.log('--------------------------------------------------');
			candidates.forEach(candidate => {
				console.log(`${candidate.name} - ${candidate.votecount} (${(candidate.votecount/totalVotes).toFixed(3)})`);
			});
			console.log('--------------------------------------------------');
		}
	}
});