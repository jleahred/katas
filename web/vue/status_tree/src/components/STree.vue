<template>
  <div class="stree">
    <ul>
      <span  v-for="(status, index) in stree" 
            :key="status.description" 
            @click.stop="switchExpanded(index)">
        <li :class="liClass(index)">
          <div :class="semaphoreColor(index)">
          </div>
          <span v-if="hasChilds(index)">
            <span v-if="isExpanded(index) === false">
              ...
            </span>
            <!-- <span v-if="isExpanded(index)">
              -
            </span>
            <span v-else>
              +
            </span> -->
          </span>
          {{status.description}}
          <div v-if="status.expanded">
            <STree :stree="status.stree"/>
          </div>
        </li>
      </span>
    </ul>
  </div>
</template>


<script>
export default {
  name: "STree",
  props: {
    stree: Array
  },
  data: function() {
    return {};
  },
  methods: {
    switchExpanded: function(index) {
      if (this.stree[index].expanded === true) {
        this.stree[index].expanded = false;
        this.$set(this.stree, index, this.stree[index]);
      } else {
        this.stree[index].expanded = true;
        this.$set(this.stree, index, this.stree[index]);
      }
    },
    hasChilds: function(index) {
      if (this.stree[index].stree.length === 0) {
        return false;
      } else {
        return true;
      }
    },
    isExpanded: function(index) {
      return this.stree[index].expanded === true;
    },
    liClass: function(index) {
      if (this.hasChilds(index)) {
        return "liHand";
      } else {
        return "liNoHand";
      }
    },
    semaphoreColor: function(index) {
      if (this.stree[index].status === "OK") {
        return "green";
      } else if (this.stree[index].status === "ERROR") {
        return "red";
      } else if (this.stree[index].status === "WARNING") {
        return "orange";
      } else {
        return "gray";
      }
    }
  }
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
h3 {
  margin: 40px 0 0;
}
ul {
  list-style-type: none;
  padding: 0;
}
li {
  padding-left: 30px;
}
.liHand {
  cursor: pointer;
}
.liNoHand {
  cursor: initial;
}
.green,
.red,
.orange {
  border-radius: 30px;
  width: 0.8em;
  height: 0.8em;
  float: left;
  vertical-align: middle;
  margin-right: 0.5em;
  margin-top: 0.1em;
}

.red {
  background: red;
}

.green {
  background: green;
}

.orange {
  background: orange;
}

/* a {
  color: #42b983;
} */
</style>
