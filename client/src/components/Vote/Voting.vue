<template>
	<div class="voting white">
		<h2>Voting</h2>
	</div>
</template>

<script>
export default {
	name: 'Voting',
	props: {
		node: String,
		blockchain: String
	},
	data() {
		return {

		}
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.voting {
	height: 150px;
}
</style>
