import Vue from 'vue';
import App from './App.vue';
import router from './router';
import store from './store/';
import axios from 'axios'
import crypto from './utils/crypto.js';
import BootstrapVue from 'bootstrap-vue'
import 'bootstrap/dist/css/bootstrap.css'
import 'bootstrap-vue/dist/bootstrap-vue.css'
import '@babel/polyfill'

// Lägg till stöd för bootstrap i vår app
Vue.use(BootstrapVue);

// Add axios to Vue instance
Vue.prototype.$http = axios.create({
	baseURL: 'http://localhost:8080/api',
	timeout: 5000
});;

Vue.config.productionTip = false;

// Create our Vue instance and attach it to #app div
new Vue({
	router,
	store,
	render: h => h(App)
}).$mount('#app');