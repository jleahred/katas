<template>
  <div class="test">
    <STree :stree="stree"/>
  </div>
</template>

<script>
// @ is an alias to /src
import STree from "@/components/STree.vue";

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
  name: "test",
  components: {
    STree
  },
  created: function() {},
  data: function() {
    return {
      stree: stree
    };
  },
  methods: {},
  beforeDestroy() {}
};
</script>
