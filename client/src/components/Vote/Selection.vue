<template>
	<div class="container">
		<h3>Select a candidate</h3>
		<select>
			<option v-for="candidate in candidates" :value="candidate.id">{{ candidate.name }}</option>
		</select>
		<button v-on:click="submit">Next</button>
	</div>
</template>

<script>
export default {
	name: 'Password',
	props: {
		submit: Function
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

</style>
