<template>
  <div class="test">
    <textarea class="big" v-model="stree_text_edit">
    </textarea>
    <button @click="update()">Update</button>
    <STree :stree="stree_data"/>
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
  created: function() {
    this.stree_text_edit = JSON.stringify(stree, null, 2);
  },
  data: function() {
    return {
      stree_data: stree,
      stree_text_edit: ""
    };
  },
  methods: {
    update() {
      this.stree_data = JSON.parse(this.stree_text_edit);
    }
  },
  computed: {
    // stree_text: {
    //   get: function() {
    //     // `this` points to the vm instance
    //     return JSON.stringify(this.stree, null, 2);
    //   },
    //   set: function(new_value) {
    //     this.stree_text_edit = new_value;
    //   }
    // }
  },
  beforeDestroy() {}
};
</script>


<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.big {
  width: 100%;
  height: 200px;
}
</style>
