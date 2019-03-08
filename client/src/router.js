/*
	I denna filen sköter vi hur strukturen på vår hemsida ska se ut
	Vi säger vilken view som ska användas på vilken address. 'localhost:8000/vote/:id -> views/Vote'
	Inte så viktigt hur de fungerar egentligen
*/
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
			component: () => import(/* webpackChunkName: "admin" */ '@/views/Vote')
		},
		{ path: '*', redirect: '/' }
	]
});
