<template>
	<div class="container">
		<h3>Password</h3>
		<input type="text" style="width: 400px;" @input="updatePW">
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

</style>
