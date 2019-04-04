<template>
	<div class="encrypt container col-md-8">
		<h3>Whitelist</h3>
		<input class="datainput" type="text" v-model="admin" placeholder="Enter secret key of admin">
		<input class="datainput" type="text" v-model="address" placeholder="Enter address to whitelist">
		<p style="margin-top: 10px;">{{ result }}</p>
		<button class="submit-button" v-on:click="submit">Run</button>
	</div>
</template>

<script>
export default {
	name: 'Whitelist',
	data() {
		return {
			admin: null,
			address: null,
			result: null
		}
	},
	methods: {
		async submit() {
			this.result = null;

			if (!this.address) return;

			try {
				await this.$store.dispatch('WHITELIST', { admin: this.admin, address: this.address });
				this.result = `Address ${this.address} whitelisted`;
			} catch (err) {
				this.result = err;
				console.error(err);
			}
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>

.encrypt {
	padding: 25px;
	display: flex;
	flex-direction: column;
	align-items: center;
}

.datainput {
	width: 90%;
	margin-top: 10px;
}

input {
	text-align: center;
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
  width: 60%;
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
