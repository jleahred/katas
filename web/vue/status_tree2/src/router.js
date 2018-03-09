import Vue from "vue";
import Router from "vue-router";
import Simple from "./views/Simple.vue";
import ITest from "./views/ITest.vue";

Vue.use(Router);

export default new Router({
  routes: [
    {
      path: "/",
      name: "simple",
      component: Simple
    },
    {
      path: "/interactive-test",
      name: "itest",
      component: ITest
    }
  ]
});
