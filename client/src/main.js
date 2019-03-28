import Vue from 'vue';
import App from './App.vue';
import router from './router';
import store from './store/';
import axios from 'axios'
import BootstrapVue from 'bootstrap-vue'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'
import '@babel/polyfill'

Vue.use(BootstrapVue);	// Lägg till stöd för bootstrap i vår app.

// Add axios to Vue instance
Vue.prototype.$http = axios.create({
	baseURL: 'http://localhost:8080/api',
	timeout: 5000
});;

Vue.config.productionTip = false;

// Ensure we clear notifications before each page load.
router.beforeEach((to, from, next) => {
	store.commit('CLEAR_NOTIFICATIONS');
	next();
});

// Create our Vue instance and attach it to #app div
new Vue({
	router,
	store,
	render: h => h(App)
}).$mount('#app');
