<template>
  <div id="app">
    <STree :stree="stree"/>
  </div>
</template>

<script>
import VueTimers from "vue-timers/mixin";
import STree from "./components/STree.vue";

var stree = [
  {
    status: "OK",
    description: "Testing on one",
    stree: [
      {
        status: "OK",
        description: "Sub level",
        stree: []
      },
      {
        status: "OK",
        description: "Second sublevel",
        stree: [
          {
            status: "OK",
            description: "sub-sub level",
            stree: []
          },
          {
            status: "OK",
            description: "subsub level second",
            stree: [
              {
                status: "ERROR",
                description: "even more nested",
                stree: []
              },
              {
                status: "WARNING",
                description: "This is WARNING",
                stree: []
              }
            ]
          }
        ]
      },
      {
        status: "OK",
        description: "on level one, option three",
        stree: []
      }
    ]
  },
  {
    status: "ERROR",
    description: "Another option",
    stree: []
  },
  {
    id: "3",
    status: "???",
    description: "Even more",
    stree: []
  }
];

export default {
  name: "app",
  components: {
    STree
  },
  created: function() {},
  data: function() {
    return {
      stree: stree,
      counter: 0
    };
  },
  methods: {
    log() {
      this.counter += 1;
      if (this.counter % 2 === 0) {
        this.stree[0].status = "ERROR";
      } else {
        this.stree[0].status = "OK";
      }
    }
  },
  beforeDestroy() {},
  mixins: [VueTimers],
  timers: {
    log: { time: 1000, autostart: true, repeat: true, callback: this.log }
  }
};
</script>

<style>
#app {
  font-family: "Avenir", Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  /* text-align: center; */
  color: #2c3e50;
  margin-top: 40px;
}
</style>
