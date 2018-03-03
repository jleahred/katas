<template>
  <div id="app">
    <STree :stree="stree"/>
  </div>
</template>

<script>
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
    status: "OK",
    description: "Another option",
    stree: []
  },
  {
    id: "3",
    status: "OK",
    description: "Even more",
    stree: []
  }
];

export default {
  name: "app",
  components: {
    STree
  },
  data: function() {
    return {
      stree: stree
    };
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
