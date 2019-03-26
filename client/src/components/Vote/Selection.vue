<template>
	<div class="container d-flex flex-column">
		<h3>Select a candidate</h3>
		<select v-model="selected">
			<option v-for="candidate in candidates" v-bind:key="candidate.id" :value="candidate.id">{{ candidate.name }}</option>
		</select>
		<button v-on:click="submit(selected)">Submit</button>
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
			selected: null
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

</style>
