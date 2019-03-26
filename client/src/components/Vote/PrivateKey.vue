<template>
	<div class="container">
		<h3>PrivateKey</h3>
		<input type="text" style="width: 400px;" @input="updatePK">
		<button v-on:click="submit">Next</button>
	</div>
</template>

<script>
export default {
	name: 'PrivateKey',
	props: {
		submit: Function
	},
	methods: {
		updatePK(input) {
			this.$store.commit('SET_PRIVATEKEY', input.target.value);
		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>

</style>
