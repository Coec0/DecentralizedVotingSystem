const Web3 = require('web3');
const bigInt = require('big-integer');
const crypto = require('./../utils/crypto.js');
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
		SET_ELECTION_NAME(state, name) {
			console.log('Election name set');
			state.electionName = name;
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
		SET_CONTRACTS(state, contracts) {
			if (contracts) {
				console.log('SmartContract instance set');
			} else if (state.smartcontract && !contracts) {
				console.log('SmartContract removed');
			}
			state.contracts = contracts;
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
		CREATE_SMARTCONTRACTS({ commit, state }, contracts) {
			return new Promise(async (resolve, reject) => {
				// Error checking
				if (typeof contracts !== 'object') return reject('payload required to be object');
				if (!state.web3) return reject('Tried CREATE_SMARTCONTRACT without web3 set');
				if (!contracts.voterecord) return reject('voterecord not in payload');
				if (!contracts.votesystem) return reject('votesystem not in payload');

				let instances = {};

				// Parse and create voterecord
				try {
					if (!contracts.voterecord.bcAddr) return reject('No smartcontract adress specified');
					if (!contracts.voterecord.abi) return reject('No smartcontract ABI specified');

					// Parse JSON string
					let parsedABI = JSON.parse(contracts.voterecord.abi);

					// Create smartcontract object and add to our instances object
					instances.voterecord = new state.web3.eth.Contract(parsedABI, contracts.voterecord.bcAddr);
				} catch (err) {
					commit('SET_CONTRACTS', null);
					return reject(err);
				}

				// Parse and create votesystem
				try {
					if (!contracts.votesystem.bcAddr) return reject('No smartcontract adress specified');
					if (!contracts.votesystem.abi) return reject('No smartcontract ABI specified');

					// Parse JSON string
					let parsedABI = JSON.parse(contracts.votesystem.abi);

					// Create smartcontract object
					instances.votesystem = new state.web3.eth.Contract(parsedABI, contracts.votesystem.bcAddr);
				} catch (err) {
					commit('SET_CONTRACTS', null);
					return reject(err);
				}

				// Add smartcontracts to state
				commit('SET_CONTRACTS', instances);

				// Tell caller to go on
				resolve(instances);
			});
		},
		FETCH_CANDIDATES({ commit, state }) {
			return new Promise(async (resolve, reject) => {
				// Error checking
				if (!state.web3) return reject('Tried FETCH_CANDIDATES without web3 set');
				if (!state.contracts) return reject('Tried FETCH_CANDIDATES without smartcontracts set');
				const candidates = [];

				try {
					const candidateCount = await state.contracts.votesystem.methods.candidateCount().call();

					for (var i = 0; i < candidateCount; i++) {
						const candidate = await state.contracts.votesystem.methods.allCandidates(i).call();

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
			commit('SET_CONTRACTS', null);
			commit('SET_PUBLICKEY', null);
		},
		SUBMIT_VOTE({ commit, state }, selection) {
			return new Promise((resolve, reject) => {
				if (!state.web3) return reject('Tried SUBMIT_VOTE without web3 set');
				if (!state.contracts) return reject('Tried SUBMIT_VOTE without smartcontracts set');

				const pk = state.publickey;

				// Encryption will happen here
				const encryptionArray = state.candidates.map(candidate => {
					return candidate.id === selection ? 1 : 0;
				});

				for (let i = 0; i < encryptionArray.length; i++) {
					encryptionArray[i] = crypto.encrypt(bigInt(encryptionArray[i]), pk.q, pk.G, pk.B, pk.p, pk.a)
				}

				console.log(encryptionArray)

				const data = encryptionArray.map(item => {
					return [ item[0].x.toString(), item[0].y.toString(), item[1].x.toString(), item[1].y.toString() ];
				});

				console.log(data);

				state.contracts.votesystem.methods.vote(data).send({ from: state.web3.eth.accounts.privateKeyToAccount(state.privatekey).address, gas: 6721975 }).then((receipt) => {
					console.log(receipt);
					resolve();
				}).catch(reject);
			});
		},
		FETCH_PUBLICKEY({ commit, state }) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried FETCH_PUBLICKEY without web3 set');
				if (!state.contracts) return reject('Tried FETCH_PUBLICKEY without smartcontracts set');

				try {
					const pk = Object.values(await state.contracts.votesystem.methods.getPublicKey().call());

					let a = bigInt(pk[0]);
					let b = bigInt(pk[1]);
					let p = bigInt(pk[2]);
					let q = bigInt(pk[3]);
					let G = { x: bigInt(pk[4]), y: bigInt(pk[5]) };
					let B = { x: bigInt(pk[6]), y: bigInt(pk[7]) };

					commit('SET_PUBLICKEY', { a, b, p, q, G, B });
					resolve();
				} catch (error) {
					reject(error);
				}
			});
		},
		WHITELIST({ commit, state }, payload) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried WHITELIST without web3 set');
				if (!state.contracts) return reject('Tried WHITELIST without smartcontracts set');

				try {
					await state.contracts.voterecord.methods.addVoterToWhitelist(payload.address).send({ from: state.web3.eth.accounts.privateKeyToAccount(payload.admin).address, gas: 6721975 })
					resolve();
				} catch (error) {
					reject(error);
				}
			});
		},
		WHITELIST_CHECK({ commit, state }, address) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried WHITELIST_CHECK without web3 set');
				if (!state.contracts) return reject('Tried WHITELIST_CHECK without smartcontracts set');

				try {
					if(await state.contracts.voterecord.methods.isOnWhiteList(address).call()) {
						resolve(`${address} is allowed to place votes`);
					} else {
						resolve(`${address} is not allowed to place votes`);
					}
				} catch (error) {
					reject(error);
				}
			});
		},
		WHITELIST_ENABLE({ commit, state }, admin) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried WHITELIST_ENABLE without web3 set');
				if (!state.contracts) return reject('Tried WHITELIST_ENABLE without smartcontracts set');

				try {
					await state.contracts.voterecord.methods.enableWhitelist().call({ from: state.web3.eth.accounts.privateKeyToAccount(admin).address});
					resolve('Whitelist enabled');
				} catch (error) {
					reject(error);
				}
			});
		},
		WHITELIST_DISABLE({ commit, state }, admin) {
			return new Promise(async (resolve, reject) => {
				if (!state.web3) return reject('Tried WHITELIST_DISABLE without web3 set');
				if (!state.contracts) return reject('Tried WHITELIST_DISABLE without smartcontracts set');

				try {
					await state.contracts.voterecord.methods.disableWhitelist().call({ from: state.web3.eth.accounts.privateKeyToAccount(admin).address});
					resolve('Whitelist disabled');
				} catch (error) {
					reject(error);
				}
			});
		}
	}
});