import Vue from "vue";
import Router from "vue-router";
import Home from "./views/Home.vue";
import Admin from "./views/Admin.vue";

Vue.use(Router);

export default new Router({
  mode: "history",
  base: process.env.BASE_URL,
  routes: [
    {
      path: "/",
      name: "home",
      component: () => import(/* webpackChunkName: "home" */ "@/views/Home")
    },
    {
      path: "/admin",
      name: "admin",
      component: () => import(/* webpackChunkName: "admin" */ "@/views/Admin")
    }
  ]
});
