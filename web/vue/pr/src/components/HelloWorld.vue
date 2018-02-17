<template>
  <div>
    <span v-bind:title="msg">
      sadfasdfsad fasdf asdf aaaa
    </span>
    <h1>{{ msg }}</h1>
    <h1>{{ msg2 + new Date().toLocaleString()}}</h1>
    <button v-on:click="switchVisible">show/hide</button>
    <ol v-if="visible">
      <li v-for="todo in todos">
        {{ todo.text }} <button v-on:click="reverseMessage">Reverse Message</button>
      </li>
    </ol>
    <input v-model="msg">
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

@Component
export default class HelloWorld extends Vue {
  @Prop() private msg!: string;
  @Prop() private msg2!: string;
  
  todos= [
    {text: "adsfasdf"},
    {text: "adsfasdf"},    
    {text: "adsfasdf"}  ];
  visible = true;
  //msg = "this is a test";
  //msg: string;

  // constructor() {
  //   super();
  // }

  switchVisible() {
    this.visible = !this.visible;
  }

  reverseMessage() {
    this.msg = this.msg.split('').reverse().join('');
  }
}
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
  /* display: inline-block; */
  margin: 0 10px;
}
a {
  color: #42b983;
}
</style>
