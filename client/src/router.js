import Vue from 'vue';
import Router from 'vue-router';

Vue.use(Router);

export default new Router({
	mode: 'history',
	base: process.env.BASE_URL,
	routes: [
		{
			path: '/home',
			name: 'home',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Home')
		},
		{
			path: '/vote/:id',
			component: () => import(/* webpackChunkName: "home" */ '@/views/Vote'),
			redirect: {
				name: 'voting'
			},
			children: [
				{
					path: 'voting',
					name: 'voting',
					component: () => import(/* webpackChunkName: "home" */ '@/views/Vote/Voting')
				},
				{
					path: 'tools',
					name: 'tools',
					component: () => import(/* webpackChunkName: "home" */ '@/views/Vote/Tools')
				},
				{
					path: 'results',
					name: 'results',
					component: () => import(/* webpackChunkName: "home" */ '@/views/Vote/Results')
				}
			]
		},
		{ path: '*', redirect: '/home' }
	]
});
