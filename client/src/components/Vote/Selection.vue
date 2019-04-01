<template>
	<div class="container d-flex flex-column align-items-center">
		<h3>Select a candidate</h3>
		<select v-model="selected">
			<option value="0" selected disabled>Choose a candidate</option>
			<option v-for="candidate in candidates" v-bind:key="candidate.id" :value="candidate.id">{{ candidate.name }}</option>
		</select>
		<button class="button" v-on:click="submit(selected)">Submit</button>
	</div>
</template>

<script>
export default {
	name: 'Password',
	props: {
		submit: Function
	},
	data() {
		return {
			selected: '0'
		}
	},
	computed: {
		candidates() {
			return this.$store.state.candidates;
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
select {
	width: 200px;
}

button {
  background:#2c3e50;
  color:#fff;
  border:none;
  position:relative;
  height:60px;
  font-size:1.6em;
  padding:0 2em;
  cursor:pointer;
  transition:800ms ease all;
  outline:none;
  margin-top: 4em;
}

button:hover {
  background:#fff;
  color:#2c3e50;
}

button:before,button:after {
  content:'';
  position:absolute;
  top:0;
  right:0;
  height:2px;
  width:0;
  background: #2c3e50;
  transition:400ms ease all;
}
button:after {
  right:inherit;
  top:inherit;
  left:0;
  bottom:0;
}
button:hover:before,button:hover:after {
  width:100%;
  transition:800ms ease all;
}
</style>
