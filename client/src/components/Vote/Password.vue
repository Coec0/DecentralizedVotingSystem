<template>
	<div class="container d-flex flex-column align-items-center">
		<h3>Password</h3>
		<input placeholder="Enter password" type="text" style="width: 400px;" @input="updatePW">
		<button v-on:click="submit">Next</button>
	</div>
</template>

<script>
export default {
	name: 'Password',
	props: {
		submit: Function
	},
	methods: {
		updatePW(input) {
			this.$store.commit('SET_PASSWORD', input.target.value);
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
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
