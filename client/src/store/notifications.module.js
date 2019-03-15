import Vue from "vue";

const initialState = {
	notifications: []
};

export const state = { ...initialState };

export const actions = { };

export const mutations = {
	ADD_NOTIFICATION(state, notification) {
		state.notifications.push(notification);
	},
	CLEAR_NOTIFICATIONS(state) {
		state.notifications = [];
	}
};

const getters = { };

export default {
	state,
	actions,
	mutations,
	getters
};