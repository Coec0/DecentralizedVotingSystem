const Web3 = require('web3');
const bigInt = require("big-integer");
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
			if (instance) {
				console.log('Web3 instance set');
			} else if (!instance && state.web3) {
				console.log('Web3 removed')
			}
			state.web3 = instance;
		},
		SET_SMARTCONTRACT(state, instance) {
			if (instance) {
				console.log('SmartContract instance set');
			} else if (state.smartcontract && !instance) {
				console.log('SmartContract removed');
			}
			state.smartcontract = instance;
		},
		SET_CANDIDATES(state, candidates) {
			if (candidates.length) {
				console.log(`Candidates set (${candidates.map(c => c.name).join(', ')})`);
			} else if (state.candidates.length && !candidates.length) {
				console.log('Candidates removed');
			}
			state.candidates = candidates;
		},
		SET_PRIVATEKEY(state, sk) {
			state.privatekey = sk;
		},
		SET_PASSWORD(state, pw) {
			state.password = pw;
		},
		SET_PUBLICKEY(state, pk) {
			state.publickey = pk;
			if (pk) {
				console.log(`Public key set`);
			} else {
				console.log(`Public key removed`);
			}
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
			commit('SET_PUBLICKEY', null);
		},
		SUBMIT_VOTE({ commit, state }, selection) {
			throw new Error('SUBMIT_VOTE not implemented');

			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried SUBMIT_VOTE without web3 set');
				if (!state.smartcontract) return reject('Tried SUBMIT_VOTE without smartcontract set');

				const pubkey = await state.smartcontract.methods.getPublicKey().call();

				// Encryption will happen here

				state.smartcontract.methods.vote(/* ENCRYPTED DATA */).send({ from: state.web3.eth.accounts.privateKeyToAccount(state.privatekey).address }).then(success => {
					console.log('Vote placed');
					resolve();
				}).catch(reject);
			});
		},
		FETCH_PUBLICKEY({ commit, state }) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried FETCH_PUBLICKEY without web3 set');
				if (!state.smartcontract) return reject('Tried FETCH_PUBLICKEY without smartcontract set');

				try {
					const pk = Object.values(await state.smartcontract.methods.getPublicKey().call());

					let a = bigInt(pk[0]);
					let b = bigInt(pk[1]);
					let p = bigInt(pk[2]);
					let q = bigInt(pk[3]);
					let G = { x: bigInt(pk[4]), y: bigInt(pk[5]) };
					let B = { x: bigInt(pk[6]), y: bigInt(pk[7]) };

					commit('SET_PUBLICKEY', { a, b, p, q, G, B });

					console.log(state.publickey)

					resolve();
				} catch (error) {
					reject(err);
				}
			});
		}
	}
});