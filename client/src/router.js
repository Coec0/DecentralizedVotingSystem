import Vue from 'vue';
import Router from 'vue-router';

Vue.use(Router);

export default new Router({
	mode: 'history',
	base: process.env.BASE_URL,
	routes: [
		{
			path: '/',
			name: 'home',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Home')
		},
		{
			path: '/vote/:id',
			name: 'vote',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Vote')
		},
		{
			path: '/tools',
			name: 'tools',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Tools')
		},
		{
			path: '/results',
			name: 'results',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Results')
		},
		{ path: '*', redirect: '/' }
	]
});
