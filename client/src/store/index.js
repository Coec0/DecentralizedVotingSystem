import Vue from 'vue';
import Vuex from 'vuex';

import vote from './vote.module';
import notifications from './notifications.module';

Vue.use(Vuex);

export default new Vuex.Store({
	modules: {
		vote,
		notifications
	}
});