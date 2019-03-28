<template>
	<router-link class="button" v-bind:to="to" tag="div"><span>{{ text }}</span></router-link>
</template>

<script>
export default {
	name: 'NavButton',
	props: {
		text: String,
		to: String
	}
};
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>
.button {
	display: flex;
	justify-content: center;
	align-items: center;
	padding: 0px 5px;
}
</style>
