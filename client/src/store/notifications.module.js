import Vue from "vue";

const initialState = {
	notifications: []
};

export const state = { ...initialState };

export const actions = { };

export const mutations = {
	ADD_NOTIFICATION(state, notification) {
		state.notifications.push(notification);
	}
};

const getters = { };

export default {
	state,
	actions,
	mutations,
	getters
};