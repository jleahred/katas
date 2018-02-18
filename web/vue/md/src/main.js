"use strict";

import "babel-core/register";
import "babel-polyfill";

import Vue from "vue";
import App from "./App.vue";
// import {
//   MdButton,
//   MdContent,
//   MdTabs,
//   MdDatepicker,
//   MdPagecontainer
// } from "vue-material/dist/components";
import VueMaterial from "vue-material";
import "vue-material/dist/vue-material.min.css";

// Vue.use(MdButton);
// Vue.use(MdContent);
// Vue.use(MdTabs);
// Vue.use(MdDatepicker);
// Vue.use(MdPagecontainer);
Vue.use(VueMaterial);

Vue.config.productionTip = false;

new Vue({
  render: h => h(App)
}).$mount("#app");
