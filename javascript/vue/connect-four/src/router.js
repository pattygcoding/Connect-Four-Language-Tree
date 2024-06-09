// src/router.js
import Vue from 'vue';
import Router from 'vue-router';
import ConnectFour from './components/ConnectFour.vue';
import TogglePage from './components/TogglePage.vue';

Vue.use(Router);

export default new Router({
  mode: 'history',
  routes: [
    { path: '/', component: ConnectFour },
    { path: '/toggle', component: TogglePage },
  ],
});
